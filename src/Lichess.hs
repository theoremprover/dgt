{-# LANGUAGE OverloadedStrings,RecordWildCards,ScopedTypeVariables,DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lichess where

import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (Status(..))
import qualified Data.ByteString.Char8 as BS
import Data.Aeson.Types (explicitParseField)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State
import Data.CaseInsensitive (mk)
import Data.Char (toLower)

import Chess200
import LichessInterface

data LichessState = LichessState {
	lisAuthCookie :: String,
	myColour      :: Colour,
	currentGameID :: Maybe String } deriving Show

type LichessM m a = StateT LichessState m a

rawLichessRequest :: (FromJSON val,MonadIO m) => String -> [(String,Maybe String)] -> [(String,String)] -> m (Response BS.ByteString,val)
rawLichessRequest path querystring headers = do
	response <- liftIO $ httpBS $
		setRequestMethod "POST" $
		setRequestPath (BS.pack path) $
		setRequestQueryString (map (\(a,b) -> (BS.pack a,fmap BS.pack b)) querystring) $
		setRequestSecure True $
		setRequestPort 443 $
		setRequestHeaders ([("Accept","application/vnd.lichess.v3+json")] ++ map (\(a,b) -> (mk (BS.pack a),BS.pack b)) headers) $
		setRequestHost "lichess.org" $
		defaultRequest	
	let bs = getResponseBody response
	case eitherDecodeStrict bs of
		Left errmsg -> do
			liftIO $ putStrLn $ "rawLichessRequest response body: " ++ BS.unpack bs
			error $ "rawLichessRequest eitherDecodeStrict: " ++ errmsg
		Right val -> return (response,val)

lichessRequestL :: (FromJSON val,MonadIO m) => String -> [(String,Maybe String)] -> LichessM m (Status,val)
lichessRequestL path querystring = do
	liftIO $ putStrLn $ "lichessRequestL path=" ++ path
	authcookie <- gets lisAuthCookie
	(response,val) <- rawLichessRequest path querystring [("Cookie",authcookie)]
--	liftIO $ BS.putStrLn $ getResponseBody response
	return (getResponseStatus response,val)

withLoginL :: (MonadIO m) => String -> String -> LichessM m a -> m a
withLoginL username password lichessm = do
	(response,user::User) <- rawLichessRequest "/login" [("username",Just username),("password",Just password)] []
	let Status{..} = getResponseStatus response
	case statusCode == 200 of
		False -> error $ "withLoginL: " ++ BS.unpack statusMessage
		True  -> do
			liftIO $ putStrLn "OK, logged in."
			let [Cookie{..}] = destroyCookieJar $ responseCookieJar response
			evalStateT lichessm $ LichessState {
				lisAuthCookie = BS.unpack cookie_name ++ "=" ++ BS.unpack cookie_value,
				myColour = White,
				currentGameID = Nothing }

startGameL :: (MonadIO m) => Maybe Position -> Maybe Colour -> LichessM m (Maybe GameData)
startGameL mb_position mb_colour = do
	(Status{..},mb_gamedata) <- lichessRequestL "/setup/ai" [
		("color",Just $ maybe "random" (map toLower . show) mb_colour),
		("days",Just "2"),("time",Just "5.0"),
		("fen",Just "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
		("increment",Just "8"),
		("level",Just "2"),
		("timeMode",Just "0"),
		("variant",Just "1") ]
	liftIO $ BS.putStrLn statusMessage
	case mb_gamedata of
		Nothing -> return ()
		Just gamedata -> do
			modify $ \ s -> s { currentGameID = Just $ LichessInterface.id ((game (gamedata::GameData))::CreatedGame) }
	return mb_gamedata

moveL :: (MonadIO m) => String -> String -> LichessM m (Status,())
moveL from to = do
	Just gameid <- gets currentGameID
	lichessRequestL ("/game/" ++ gameid) [
		("orig",Just from),("dest",Just to),
		("fen",Just "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
		("variant",Just "standard") ]
