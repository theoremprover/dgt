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

import           Control.Concurrent.Lifted
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

startGameL :: Maybe Position -> Maybe Colour -> LichessM GameData
startGameL mb_position mb_colour = do
	liftIO $ putStrLn "startGameL..."
	let pos = maybe initialPosition Prelude.id mb_position
	(status,gamedata) <- lichessRequestL "POST" "lichess.org" "/setup/ai" [
		("color",Just $ maybe "random" (map toLower . show) mb_colour),
		("days",Just "2"),("time",Just "5.0"),
		("fen",Just $ toFEN pos),
		("increment",Just "8"),
		("level",Just "2"),
		("timeMode",Just "0"),
		("variant",Just "1") ] []
	liftIO $ print status
	return gamedata

joinGameL :: String -> LichessM GameData
joinGameL gameid = do
	(status,gamedata) <- lichessRequestL "GET" "lichess.org" ("/"++gameid) [] []
	liftIO $ print status
	return gamedata

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

inGameL :: GameData -> InGameM () -> LichessM (Position,Colour)
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
	context <- liftIO $ initConnectionContext -- TODO: Einfacher, gegebene Funktionen benutzen?
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
		ingamem
--		liftIO $ sendClose conn ()   -- TODO: Where to close conn?

	let Right pos@Position{..} = fromFEN (fen (cur_game::CreatedGame))
	return (pos,mycolour)

pingG :: InGameM ()
pingG = do
	InGameState{..} <- sharedGet
	when (isNothing igsLastPing) $ do
		sendG $ LichessMsg (Just 0) "p" Nothing
		now <- liftIO $ getTime Monotonic
		sharedModify $ \ s -> s { igsLastPing = Just now }

sendG :: LichessMsg -> InGameM ()
sendG lichessmsg = withSharedStateAtomically $ \ InGameState{..} -> do
--	liftIO $ putStrLn $ "sendG " ++ show (toJSON lichessmsg)
	liftIO $ WS.sendTextData igsConnection lichessmsg
--	sharedModify $ \ s -> s { igsMyMoveSent = True }
	logMsg $ "SENT: " ++ show lichessmsg ++ "\n"

receiveG :: InGameM LichessMsg
receiveG = withSharedState $ \ InGameState{..} -> do
	datamsg <- liftIO $ receiveDataMessage igsConnection
	let (lichessmsg,x) :: (LichessMsg,String) = case datamsg of
		Text   x -> (fromLazyByteString x,BSL8.unpack x)
		Binary x -> (fromLazyByteString x,BSL8.unpack x)
--	liftIO $ putStrLn $ "receiveG: " ++ x
--	lichessmsg <- liftIO $ WS.receiveData igsConnection
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

listenForCommandsG :: LichessChan -> InGameM ()
listenForCommandsG inputchan = forever $ do
	command <- readChan inputchan
	liftIO $ putStrLn $ "listenForCommandsG: Got " ++ show command
	case command of
		SendMove move -> sendMoveG move

listenForLichessG :: ConfluenceChan -> InGameM ()
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
--						liftIO $ putStrLn $ "### Lag = " ++ show (div lag_ns 1000000) ++ " ms"
						sharedModify $ \ s -> s { igsLastPing = Nothing }
					Nothing -> return ()
				return Nothing
			(_,Just (PEndData (EndData winnerordraw gamestatus))) -> do
				sharedModify $ \ s -> s { igsGameInProgress = False }
				return $ Just $ LichessGameEnd $ case winnerordraw of
					WDWinner colour -> Winner colour Checkmate  -- TODO: WinReason herausfinden
					WDDraw          -> Draw NoMatePossible      -- TODO: DrawReason herausfinden
			(_,Just (PCrowd crowd)) -> return Nothing
			(t,Just (PUnknown v)) -> do
				liftIO $ putStrLn $ "###### listenForLichessG: Could not parse t=" ++ show t ++ ": " ++ show v
				return Nothing
			unknown -> do
				liftIO $ putStrLn $ "###### listenForLichessG: No impl. for " ++ show unknown
				return Nothing

		case mb_msg of
			Nothing  -> return ()
			Just msg -> do
				writeChan outputchan msg
				logMsg $ "messageLoopG: writeChan " ++ show msg

forkLichessThread :: String -> String -> Chan LichessCommand -> Chan ConfluenceMsg -> IO (Position,Colour)
forkLichessThread username pw inputchan outputchan = withLoginL username pw $ \ user -> do
	gamedata <- case nowPlaying (user::User) of
		Just (game:_) -> joinGameL (gameId game)
		_             -> startGameL Nothing (Just White)
	inGameL gamedata $ do
		fork $ ( do
			forever $ do
				pingG
				threadDelay (1500*1000) )
			`catchIO` (const $ return ())
		fork $ listenForCommandsG inputchan
		fork $ listenForLichessG outputchan
		return ()