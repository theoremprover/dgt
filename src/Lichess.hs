{-# LANGUAGE OverloadedStrings,RecordWildCards,ScopedTypeVariables,DuplicateRecordFields,UnicodeSyntax,FlexibleInstances,UndecidableInstances,IncoherentInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lichess where

import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (Status(..))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Network.Connection
import Network.WebSockets as WS
import Network.WebSockets.Stream
import Data.Aeson.Types (explicitParseField)
import Control.Monad
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State.Strict
import Data.CaseInsensitive (mk)
import Data.Char (toLower)
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Network.Socket (withSocketsDo)
import System.Random
import Wuss
import Network.TLS
import Data.Maybe

import Chess200
import LichessInterface
import FEN


data LichessState = LichessState {
	clientID      :: String,
	lisAuthCookie :: String,
	myColour      :: Colour,
	socketURL     :: Maybe String,
	currentGameID :: Maybe String,
	currentPos    :: Maybe Position } deriving Show

type LichessM a = StateT LichessState IO a

rawLichessRequest :: (FromJSON val,MonadIO m) => BS.ByteString -> String -> [(String,Maybe String)] -> [(String,String)] -> m (Network.HTTP.Conduit.Response BS.ByteString,val)
rawLichessRequest host path querystring headers = do
	response <- liftIO $ httpBS $
		setRequestMethod "POST" $
		setRequestPath (BS.pack path) $
		setRequestQueryString (map (\(a,b) -> (BS.pack a,fmap BS.pack b)) querystring) $
		setRequestSecure True $
		setRequestPort 443 $
		setRequestHeaders ([("Accept","application/vnd.lichess.v3+json")] ++ map (\(a,b) -> (mk (BS.pack a),BS.pack b)) headers) $
		setRequestHost host $
		defaultRequest	
	let bs = getResponseBody response
	case eitherDecodeStrict bs of
		Left errmsg -> do
			liftIO $ putStrLn $ "rawLichessRequest response body: " ++ BS.unpack bs
			error $ "rawLichessRequest eitherDecodeStrict: " ++ errmsg
		Right val -> return (response,val)

lichessRequestL :: (FromJSON val) => BS.ByteString -> String -> [(String,Maybe String)] -> [(String,String)] -> LichessM (Status,val)
lichessRequestL host path querystring headers = do
	liftIO $ putStrLn $ "lichessRequestL path=" ++ show path
	authcookie <- gets lisAuthCookie
	liftIO $ putStrLn $ "authcookie=" ++ authcookie
	(response,val) <- rawLichessRequest host path querystring (("Cookie",authcookie):headers)
	let status = getResponseStatus response
	liftIO $ BS.putStrLn $ statusMessage status
	liftIO $ BS.putStrLn $ getResponseBody response
	return (status,val)

withLoginL :: String -> String -> LichessM a -> IO a
withLoginL username password lichessm = withSocketsDo $ do
	(response,user::User) <- rawLichessRequest "lichess.org" "/login" [("username",Just username),("password",Just password)] []
	let Status{..} = getResponseStatus response
	case statusCode == 200 of
		False -> error $ "withLoginL: " ++ BS.unpack statusMessage
		True  -> do
			liftIO $ putStrLn "OK, logged in."
			let [Cookie{..}] = destroyCookieJar $ responseCookieJar response
			clientid <- forM [1..10] $ \ _ -> liftIO $ getStdRandom (randomR ('a','z'))
			evalStateT lichessm $ LichessState {
				clientID      = clientid,
				lisAuthCookie = BS.unpack cookie_name ++ "=" ++ BS.unpack cookie_value,
				myColour      = White,
				socketURL     = Nothing,
				currentGameID = Nothing,
				currentPos    = Nothing }

startGameL :: Maybe Position -> Maybe Colour -> LichessM (Maybe GameData)
startGameL mb_position mb_colour = do
	liftIO $ putStrLn "startGameL..."
	let pos = maybe initialPosition Prelude.id mb_position
	(Status{..},mb_gamedata) <- lichessRequestL "lichess.org" "/setup/ai" [
		("color",Just $ maybe "random" (map toLower . show) mb_colour),
		("days",Just "2"),("time",Just "5.0"),
		("fen",Just $ toFEN pos),
		("increment",Just "8"),
		("level",Just "2"),
		("timeMode",Just "0"),
		("variant",Just "1") ] []
	case mb_gamedata of
		Nothing -> return ()
		Just gamedata -> do
			modify $ \ s -> s {
				currentGameID = Just $ LichessInterface.id ((game (gamedata::GameData))::CreatedGame),
				socketURL     = Just $ socket (url gamedata),
				currentPos    = Just pos }
	return mb_gamedata

joinGameL :: String -> InGameL a -> LichessM a
joinGameL gameid ingamel = do
	liftIO $ putStrLn $ "joinGameL gameid=" ++ gameid
--	LichessState{..} <- get
	(status@Status{..},gamedata::GameData) <- lichessRequestL "https://lichess.org" ("/api/game/"++gameid) "" []
	modify $ \ s -> s { 
	inGameL ingamel

type InGameM a = StateT InGameState (StateT LichessState IO) a

data InGameState = InGameState {
	igsConnection :: WS.Connection }

inGameL :: InGameM a -> LichessM a
inGameL ingamem = do
	LichessState{..} <- get
	liftIO $ putStrLn $ "inGameL..."		
	let path = fromJust socketURL ++ "?sri=" ++ clientID ++ "&version=" ++ socketVersion
	liftIO $ putStrLn $ "path=" ++ path
	let (host,port,options,headers) = ("socket.lichess.org",9021,defaultConnectionOptions,[])
	context <- liftIO $ initConnectionContext
	connection <- liftIO $ connectTo context $ ConnectionParams {
		connectionHostname = host, connectionPort = port,
		connectionUseSecure = Just $ TLSSettingsSimple {
			settingDisableCertificateValidation = True,
			settingDisableSession = False,
			settingUseServerName = False },
		connectionUseSocks = Nothing }
	stream <- liftIO $ makeStream
		(fmap Just (connectionGetChunk connection))
		(maybe (return ()) (connectionPut connection . BSL.toStrict))
	conn <- liftIO $ WS.runClientWithStream stream host path options headers return
	evalStateT ingamem $ InGameState conn 

instance (FromJSON a,ToJSON a) => WebSocketsData a where
	fromLazyByteString = either error Prelude.id . eitherDecode
	toLazyByteString   = encode

sendG :: (WebSocketsData a) => a -> InGameM ()
sendG a = do
	conn <- gets igsConnection
	liftIO $ WS.sendTextData conn a

doMoveG :: Move -> InGameM ()
doMoveG Move{..} = do
	sendG $ LiMove (show moveFrom) (show moveTo) $ case movePromote of
		Nothing -> Nothing
		Just Ú -> Just "knight"
		Just Û -> Just "bishop"
		Just Ü -> Just "rook"
		Just Ý -> Just "queen"
