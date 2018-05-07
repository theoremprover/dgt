{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lichess where

import           Control.Concurrent.Lifted        as L
import           Control.Exception
import           Control.Exception.Enclosed
import           Control.Monad
import           Control.Monad.Loops (untilM_)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.State.Strict
import           Data.Aeson
import           Data.Aeson.Types                 (explicitParseField)
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as BSL8
import qualified Data.ByteString.Lazy             as BSL
import           Data.CaseInsensitive             (mk)
import           Data.Char                        (toLower)
import           Data.Maybe
import           Network.Connection
import           Network.HTTP.Conduit
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status        (Status (..))
import           Network.Socket                   (withSocketsDo)
import           Network.TLS
import           Network.WebSockets             as WS
import           Network.WebSockets.Connection  (receiveDataMessage)
import           Network.WebSockets.Stream
import           System.Random
import           Wuss

import           Chess200
import           FEN
import           LichessInterface


data LichessState = LichessState {
	clientID      :: String,
	lisAuthCookie :: String } deriving Show

type LichessM a = StateT LichessState IO a

rawLichessRequest :: (FromJSON val,MonadIO m) => BS.ByteString -> BS.ByteString -> String -> [(String,Maybe String)] -> [(String,String)] -> m (Network.HTTP.Conduit.Response BS.ByteString,val)
rawLichessRequest method host path querystring headers = do
--	liftIO $ putStrLn $ "rawLichessRequest method=" ++ BS.unpack method ++ " host=" ++ BS.unpack host ++ " path=" ++ path ++ " querystring=" ++ show querystring
	response <- liftIO ( ( httpBS $
		setRequestMethod method $
		setRequestPath (BS.pack path) $
		setRequestQueryString (map (\(a,b) -> (BS.pack a,fmap BS.pack b)) querystring) $
		setRequestSecure True $
		setRequestPort 443 $
		setRequestHeaders ([("Accept","application/vnd.lichess.v3+json")] ++ map (\(a,b) -> (mk (BS.pack a),BS.pack b)) headers) $
		setRequestHost host $
		defaultRequest ) `catch` \case
			ex@(HttpExceptionRequest _ excontent) -> error ("XXX:" ++ show ex)
			ex -> error $ show ex )
	let bs = getResponseBody response
	case eitherDecodeStrict bs of
		Left errmsg -> do
			liftIO $ putStrLn $ "rawLichessRequest response body: " ++ BS.unpack bs
			error $ "rawLichessRequest eitherDecodeStrict: " ++ errmsg
		Right val -> return (response,val)

lichessRequestL :: (FromJSON val) => BS.ByteString -> BS.ByteString -> String -> [(String,Maybe String)] -> [(String,String)] -> LichessM (Status,val)
lichessRequestL method host path querystring headers = do
	authcookie <- gets lisAuthCookie
	(response,val) <- rawLichessRequest method host path querystring (("Cookie",authcookie):headers)
	let status = getResponseStatus response
	liftIO $ BS.putStrLn $ statusMessage status
	liftIO $ BS.putStrLn $ getResponseBody response
	return (status,val)

withLoginL :: String -> String -> (User -> LichessM a) -> IO a
withLoginL username password lichessm = withSocketsDo $ do
	writeFile "msgs.log" ""
	(response,user::User) <- rawLichessRequest "POST" "lichess.org" "/login" [("username",Just username),("password",Just password)] []
	let Status{..} = getResponseStatus response
	case statusCode == 200 of
		False -> error $ "withLoginL: " ++ BS.unpack statusMessage
		True  -> do
			liftIO $ putStrLn "OK, logged in."
			let [Cookie{..}] = destroyCookieJar $ responseCookieJar response
			clientid <- forM [1..10] $ \ _ -> liftIO $ getStdRandom (randomR ('a','z'))
			evalStateT (lichessm user) $ LichessState {
				clientID      = clientid,
				lisAuthCookie = BS.unpack cookie_name ++ "=" ++ BS.unpack cookie_value }

startGameL :: Maybe Position -> Maybe Colour -> InGameM a -> LichessM a
startGameL mb_position mb_colour ingamem = do
	liftIO $ putStrLn "startGameL..."
	let pos = maybe initialPosition Prelude.id mb_position
	(Status{..},gamedata) <- lichessRequestL "POST" "lichess.org" "/setup/ai" [
		("color",Just $ maybe "random" (map toLower . show) mb_colour),
		("days",Just "2"),("time",Just "5.0"),
		("fen",Just $ toFEN pos),
		("increment",Just "8"),
		("level",Just "2"),
		("timeMode",Just "0"),
		("variant",Just "1") ] []
	inGameL gamedata ingamem

joinGameL :: String -> InGameM a -> LichessM a
joinGameL gameid ingamem = do
	(status@Status{..},gamedata) <- lichessRequestL "GET" "lichess.org" ("/"++gameid) [] []
	inGameL gamedata ingamem

type InGameM a = StateT InGameState (StateT LichessState IO) a

data InGameState = InGameState {
	igsConnection     :: WS.Connection,
	igsMyColour       :: Colour,
	igsCurrentPos     :: Position,
	igsSocketURL      :: String,
	igsCurrentGameID  :: String,
	igsMyNextMove     :: Int,
	igsGameInProgress :: Bool }
	deriving Show
instance Show WS.Connection where
	show _ = "<SOME CONNECTION>"

inGameL :: GameData -> InGameM a -> LichessM a
inGameL gamedata ingamem = do
	let
		cur_game      = game (gamedata::GameData)
		currentgameid = LichessInterface.id (cur_game :: CreatedGame)
		socketurl     = LichessInterface.round (url gamedata)
		mycolour      = player (cur_game::CreatedGame)
	liftIO $ appendFile "msgs.log" (show gamedata ++ "\n")
	LichessState{..} <- get
	let path = socketurl ++ "/socket/v2?sri=" ++ clientID
	liftIO $ putStrLn $ "path=" ++ path
	let
		(host,port,options) = ("socket.lichess.org",443,defaultConnectionOptions)
		headers = [ ("Cookie",BS.pack lisAuthCookie) ]
	liftIO $ print headers
	context <- liftIO $ initConnectionContext
	connection <- liftIO $ connectTo context $ ConnectionParams {
		connectionHostname = host,
		connectionPort     = port,
		connectionUseSecure = Just $ TLSSettingsSimple {
			settingDisableCertificateValidation = False,
			settingDisableSession               = False,
			settingUseServerName                = False },
		connectionUseSocks = Nothing }
	stream <- liftIO $ makeStream
		(fmap Just (connectionGetChunk connection))
		(maybe (return ()) (connectionPut connection . BSL.toStrict))
	conn <- liftIO $ WS.runClientWithStream stream host path options headers return
	liftIO $ appendFile "msgs.log" $ "FEN: " ++ fen (cur_game::CreatedGame)
	let
		Right pos@Position{..} = fromFEN (fen (cur_game::CreatedGame))
		mynextmove = pNextMoveNumber + if pColourToMove == Black && mycolour == White then 1 else 0
	flip evalStateT (InGameState conn mycolour pos socketurl currentgameid mynextmove True) $ do
		L.fork $ ( do
			forever $ do
				pingG
				L.threadDelay (1500*1000) )
			`catchIO` (const $ return ())
		ret <- ingamem
		liftIO $ sendClose conn ()
		return ret

pingG :: InGameM ()
pingG = do
	liftIO $ putStrLn "Ping"
	sendG $ LichessMsg (Just 0) "p" Nothing

sendG :: LichessMsg -> InGameM ()
sendG lichessmsg = do
	liftIO $ putStrLn $ "sendG " ++ show (toJSON lichessmsg)
	conn <- gets igsConnection
	liftIO $ WS.sendTextData conn lichessmsg
	liftIO $ do
		appendFile "msgs.log" $ "SENT: " ++ show lichessmsg ++ "\n"

receiveG :: InGameM LichessMsg
receiveG = do
	conn <- gets igsConnection
	datamsg <- liftIO $ receiveDataMessage conn
	let (lichessmsg,x) :: (LichessMsg,String) = case datamsg of
		Text   x -> (fromLazyByteString x,BSL8.unpack x)
		Binary x -> (fromLazyByteString x,BSL8.unpack x)
	liftIO $ putStrLn $ "receiveG: " ++ x
--	lichessmsg <- liftIO $ WS.receiveData conn
	liftIO $ putStrLn $ "receiveG: " ++ show lichessmsg
	liftIO $ do
		appendFile "msgs.log" $ "RECV: " ++ show lichessmsg ++ "\n"
	return lichessmsg

sendMoveG :: Move -> InGameM ()
sendMoveG Move{..} = do
	sendG $ LichessMsg Nothing "move" $ Just $ PMyMove $ MyMove $ MoveFromTo moveFrom moveTo movePromote

messageLoopG = do
	msg <- receiveG
	breakloop <- handleMessageG msg
	when (not breakloop) messageLoopG

handleMessageG (LichessMsg _ t mb_payload) = do
	case (t,mb_payload) of
		(_,Just (PMessages msgs)) -> sequence_ $ map handleMessageG msgs
		(_,Just (PLiMove limove)) -> doMoveG limove
		(_,Just (PEndData _))     -> modify $ \ s -> s { igsGameInProgress = False }
		_                         -> return ()
	InGameState{..} <- get
	let pos@Position{..} = igsCurrentPos
	let nowmetomove = pColourToMove==igsMyColour && pNextMoveNumber==igsMyNextMove
	when nowmetomove $ liftIO $ print pos
	return nowmetomove
	
doMoveG :: LiMove -> InGameM ()
doMoveG limove = do
	let MoveFromTo from to mb_promote = uci (limove::LiMove)
	pos <- gets igsCurrentPos
	let move = case [ move | move@Move{..} <- moveGen pos, moveFrom==from, moveTo==to, movePromote==mb_promote ] of
		move:_ -> move
		_ -> error $ "limove=" ++ show limove ++ "\nmoveGen pos = " ++ show (moveGen pos)
	modify $ \ s -> s { igsCurrentPos = doMove (igsCurrentPos s) move }
	cpos <- gets igsCurrentPos
	liftIO $ print cpos
{-
receiveG: {"t":"b","d":[
	{"v":8,"t":"move","d":{
		"uci":"c5f2",
		"san":"Bxf2#",
		"fen":"r1b1k1nr/ppp2ppp/2n1pq2/3p4/8/P7/RPPPPbPP/1NBQKBNR",
		"ply":12,
		"dests":null,
		"status":{"id":30,"name":"mate"},
		"winner":"black","check":true}},
	{"v":9,"t":"end","d":"black"},
	{"v":10,"t":"endData","d":{"winner":"black","status":{"id":30,"name":"mate"}}}
]}

{"v":9,"t":"move","d":{
	"uci":"h2h3",
	"san":"h3",
	"fen":"r2qkbnr/ppp2ppp/2np4/1B2p3/4P1b1/P4N1P/1PPP1PP1/RNBQK2R",
	"ply":9,
	"dests":{"a8":"b8c8","f8":"e7","e8":"e7d7","f7":"f6f5","d8":"d7c8b8e7f6g5h4","g7":"g6g5","b7":"b6","a7":"a6a5","d6":"d5","h7":"h6h5","g4":"f5e6d7c8h5f3h3","g8":"f6h6e7"
	}}}
-}
