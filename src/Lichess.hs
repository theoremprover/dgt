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
--import           Control.Monad.Loops (untilM_)
--import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Control.Monad.Trans.Control
--import           Control.Monad.Trans.State.Strict
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
import System.Clock
--import Control.Concurrent.MVar.Lifted
import Control.Concurrent.Chan.Lifted

import Log
import Chess200
import FEN
import LichessInterface
import SharedState
import Confluence

type LichessChan = Chan LichessCommand

data LichessState = LichessState {
	clientID      :: String,
	lisAuthCookie :: String } deriving Show

type LichessM = SharedStateT LichessState IO

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
	authcookie <- sharedGets lisAuthCookie
	(response,val) <- rawLichessRequest method host path querystring (("Cookie",authcookie):headers)
	let status = getResponseStatus response
	liftIO $ BS.putStrLn $ statusMessage status
	liftIO $ BS.putStrLn $ getResponseBody response
	return (status,val)

withLoginL :: String -> String -> (User -> LichessM a) -> IO a
withLoginL username password lichessm = withSocketsDo $ do
	(response,user::User) <- rawLichessRequest "POST" "lichess.org" "/login" [("username",Just username),("password",Just password)] []
	let Status{..} = getResponseStatus response
	case statusCode == 200 of
		False -> error $ "withLoginL: " ++ BS.unpack statusMessage
		True  -> do
			liftIO $ putStrLn "OK, logged in."
			let [Cookie{..}] = destroyCookieJar $ responseCookieJar response
			clientid <- forM [1..10] $ \ _ -> liftIO $ getStdRandom (randomR ('a','z'))
			evalSharedStateT (lichessm user) $ LichessState {
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

type InGameM = SharedStateT InGameState (SharedStateT LichessState IO)

data InGameState = InGameState {
	igsConnection     :: WS.Connection,
--	igsMyColour       :: Colour,
--	igsCurrentPos     :: Position,
	igsSocketURL      :: String,
	igsCurrentGameID  :: String,
--	igsMyNextMove     :: Int,
	igsGameInProgress :: Bool,
--	igsMyMoveSent     :: Bool,
	igsLastPing       :: Maybe TimeSpec }
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
	logMsg $ show gamedata ++ "\n"
	LichessState{..} <- sharedGet
	let path = socketurl ++ "/socket/v2?sri=" ++ clientID
	liftIO $ putStrLn $ "path=" ++ path
	let
		(host,port,options) = ("socket.lichess.org",443,defaultConnectionOptions)
		headers = [ ("Cookie",BS.pack lisAuthCookie) ]
	liftIO $ print headers
	context <- liftIO $ initConnectionContext
	connection <- liftIO $ connectTo context $ ConnectionParams {
		connectionHostname  = host,
		connectionPort      = port,
		connectionUseSecure = Just $ TLSSettingsSimple {
			settingDisableCertificateValidation = False,
			settingDisableSession               = False,
			settingUseServerName                = False },
		connectionUseSocks  = Nothing }
	stream <- liftIO $ makeStream
		(fmap Just (connectionGetChunk connection))
		(maybe (return ()) (connectionPut connection . BSL.toStrict))
	conn <- liftIO $ WS.runClientWithStream stream host path options headers return
	logMsg $ "FEN: " ++ fen (cur_game::CreatedGame)
{-
	let
		Right pos@Position{..} = fromFEN (fen (cur_game::CreatedGame))
		mynextmove = pNextMoveNumber + if pColourToMove == Black && mycolour == White then 1 else 0
-}
	flip evalSharedStateT (InGameState conn socketurl currentgameid True Nothing) $ do
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
	mb_lastping <- sharedGets igsLastPing
	when (isNothing mb_lastping) $ do
		sendG $ LichessMsg (Just 0) "p" Nothing
		now <- liftIO $ getTime Monotonic
		sharedModify $ \ s -> s { igsLastPing = Just now }

sendG :: LichessMsg -> InGameM ()
sendG lichessmsg = do
	liftIO $ putStrLn $ "sendG " ++ show (toJSON lichessmsg)
	conn <- sharedGets igsConnection
	liftIO $ WS.sendTextData conn lichessmsg
--	sharedModify $ \ s -> s { igsMyMoveSent = True }
	logMsg $ "SENT: " ++ show lichessmsg ++ "\n"

receiveG :: InGameM LichessMsg
receiveG = do
	conn <- sharedGets igsConnection
	datamsg <- liftIO $ receiveDataMessage conn
	let (lichessmsg,x) :: (LichessMsg,String) = case datamsg of
		Text   x -> (fromLazyByteString x,BSL8.unpack x)
		Binary x -> (fromLazyByteString x,BSL8.unpack x)
--	liftIO $ putStrLn $ "receiveG: " ++ x
--	lichessmsg <- liftIO $ WS.receiveData conn
--	liftIO $ putStrLn $ "receiveG: " ++ show lichessmsg
	logMsg $ "RECV = " ++ x ++ "\n"
	logMsg $ "RECV: " ++ show lichessmsg ++ "\n"
	return lichessmsg

sendMoveG :: Move -> InGameM ()
sendMoveG Move{..} = do
	sendG $ LichessMsg Nothing "move" $ Just $ PMyMove $ MyMove $ MoveFromTo moveFrom moveTo movePromote

{-
doMoveG :: LiMove -> InGameM ()
doMoveG limove = do
	let MoveFromTo from to mb_promote = uci (limove::LiMove)
	pos <- gets igsCurrentPos
	let move = case [ move | move@Move{..} <- moveGen pos, moveFrom==from, moveTo==to, movePromote==mb_promote ] of
		move:_ -> move
		_ -> error $ "limove=" ++ show limove ++ "\nmoveGen pos = " ++ show (moveGen pos)
	sharedModify $ \ s -> s {
		igsCurrentPos = doMove (igsCurrentPos s) move,
		igsMyNextMove = igsMyNextMove s + if pColourToMove (igsCurrentPos s) == igsMyColour s then 1 else 0 }
	igs <- get
	liftIO $ print (igsCurrentPos igs)
	logMsg $ "MOVED:\n" ++ show igs ++ "\n"
-}

data LichessCommand =
	SendMove Move
	deriving Show

listenForCommandsG inputchan = forever $ do
	command <- readChan inputchan
	liftIO $ putStrLn $ "listenForCommandsG: Got " ++ show command
	case command of
		SendMove move -> sendMoveG move

listenForLichessG outputchan = forever $ receiveG >>= handlemsg
	where
	handlemsg (LichessMsg _ t mb_payload) = do
		mb_msg <- case (t,mb_payload) of
			(_,Just (PMessages msgs)) -> sequence_ (map handlemsg msgs) >> return Nothing
			(_,Just (PLiMove limove)) -> return $ Just $ LichessMove $ uci (limove::LiMove)
{-
				Position{..} <- gets igsCurrentPos
				when (pNextMoveNumber*2-1 + (if pColourToMove==White then 0 else 1) == ply (limove::LiMove)) $ do
					doMoveG limove
					sharedModify $ \ s -> s { igsMyMoveSent = False }
-}
			("n",_) -> do
				now <- liftIO $ getTime Monotonic
				mb_lastping <- sharedGets igsLastPing
				case mb_lastping of
					Just lastping -> do
						let lag_ns = toNanoSecs $ diffTimeSpec now lastping
						liftIO $ putStrLn $ "### Lag = " ++ show (div lag_ns 1000000) ++ " ms"
						sharedModify $ \ s -> s { igsLastPing = Nothing }
					Nothing -> return ()
				return Nothing
			(_,Just (PEndData (EndData winnerordraw gamestatus)))     -> do
				sharedModify $ \ s -> s { igsGameInProgress = False }
				return $ Just $ LichessGameEnd $ case winnerordraw of
					WDWinner colour -> Winner colour Checkmate  -- TODO: WinReason herausfinden
					WDDraw          -> Draw NoMatePossible      -- TODO: DrawReason herausfinden
			_ -> return Nothing

		case mb_msg of
			Nothing  -> return ()
			Just msg -> do
				writeChan outputchan msg
				logMsg $ "messageLoopG: writeChan " ++ show msg

{-
		InGameState{..} <- get
		let pos@Position{..} = igsCurrentPos
		let nowmetomove = pColourToMove==igsMyColour && pNextMoveNumber==igsMyNextMove
		return nowmetomove
-}	

lichessThread :: String -> String -> Chan LichessCommand -> Chan ConfluenceMsg -> IO ()
lichessThread username pw inputchan outputchan = do
	withLoginL username pw $ \ user -> do
		case nowPlaying (user::User) of
			Just (game:_) -> joinGameL (gameId game) $ listenForLichessG outputchan
			_             -> startGameL Nothing (Just White) $ listenForLichessG outputchan
		fork $ listenForCommandsG inputchan
	where

	listen_main = do
		command <- readChan inputchan
		msg <- case command of
			
{-
	gameloop = do
		igs <- sharedGet
		let pos@Position{..} = igsCurrentPos igs
		liftIO $ appendFile "msgs.log" $ show igs ++ "\n"
		when ( pColourToMove == igsMyColour igs && pNextMoveNumber == igsMyNextMove igs && not (igsMyMoveSent igs)) $ do
			let move:_ = moveGen pos
			sendMoveG move
		messageLoopG
		inprogress <- sharedGets igsGameInProgress
		case inprogress of
			True  -> gameloop
			False -> liftIO $ putStrLn "GAME END."
-}
