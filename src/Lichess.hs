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
import           Control.Monad.Loops
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
import System.Clock

import Log
import Chess200
import FEN
import LichessInterface

withLichessDo = withSocketsDo

data LichessState = LichessState {
	clientID      :: String,
	lisAuthCookie :: String } deriving Show

type LichessM = StateT LichessState

rawLichessRequest :: (FromJSON val,MonadIO m) => BS.ByteString -> BS.ByteString -> String -> [(String,Maybe String)] ->
	[(String,String)] -> m (Network.HTTP.Conduit.Response BS.ByteString,val)
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
			ex@(SomeException _) -> error $ "rawLichessRequest httpBS:" ++ show ex )
	let bs = getResponseBody response
	case eitherDecodeStrict bs of
		Left errmsg -> do
			liftIO $ putStrLn $ "rawLichessRequest response body: " ++ BS.unpack bs
			error $ "rawLichessRequest eitherDecodeStrict: " ++ errmsg
		Right val -> return (response,val)

lichessRequestL :: (FromJSON val,MonadIO m) => BS.ByteString -> BS.ByteString -> String ->
	[(String,Maybe String)] -> [(String,String)] -> LichessM m (Status,val)
lichessRequestL method host path querystring headers = do
	authcookie <- gets lisAuthCookie
	(response,val) <- rawLichessRequest method host path querystring (("Cookie",authcookie):headers)
	let status = getResponseStatus response
	liftIO $ BS.putStrLn $ statusMessage status
	liftIO $ BS.putStrLn $ getResponseBody response
	return (status,val)

withLoginL :: (MonadIO m) => String -> String -> (User -> LichessM m a) -> m a
withLoginL username password lichessm = do
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

startGameL :: (MonadIO m) => Maybe Position -> Maybe Colour -> LichessM m String
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
	return $ LichessInterface.id (game (gamedata::GameData) :: CreatedGame)

getGameDataL :: (MonadIO m) => String -> LichessM m GameData
getGameDataL gameid = do
	(status,gamedata) <- lichessRequestL "GET" "lichess.org" ("/"++gameid) [] []
	liftIO $ print status
	return gamedata

type InGameM = StateT InGameState

data InGameState = InGameState {
	igsConnection     :: WS.Connection,
	igsSocketURL      :: String,
	igsCurrentGameID  :: String,
	igsLastPing       :: Maybe TimeSpec,
	igsMyColour       :: Colour,
	igsPosition       :: Position
 }
	deriving Show
instance Show WS.Connection where
	show _ = "<SOME CONNECTION>"

inGameL :: (MonadIO m,MonadBaseControl IO m) => String -> InGameM (LichessM m) a -> LichessM m a
inGameL gameid ingamem = do
	gamedata <- getGameDataL gameid
	let
		cur_game      = game (gamedata::GameData)
		socketurl     = LichessInterface.round (url gamedata)
		mycolour      = player (cur_game::CreatedGame)
		Right pos@Position{..} = fromFEN (fen (cur_game::CreatedGame))
	logMsg $ "FEN: " ++ fen (cur_game::CreatedGame)
	logMsg $ show gamedata ++ "\n"
	LichessState{..} <- get
	let
		path        = socketurl ++ "/socket/v2?sri=" ++ clientID
		(host,port) = ("socket.lichess.org",443)
		headers     = [ ("Cookie",BS.pack lisAuthCookie) ]
--	liftIO $ print headers

	ex_or_result <- tryIO $ do

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
		conn <- liftIO $ WS.runClientWithStream stream host path defaultConnectionOptions headers return

		flip evalStateT (InGameState conn socketurl gameid Nothing mycolour pos) $ do
			pingthreadid <- fork $ do
				forever $ do
					threadDelay (1500*1000)
					pingG
				`catchIO` (\ ex -> liftIO $ putStrLn $ "pingthread died: " ++ show ex)

			ingamem

	case ex_or_result of
		Right a -> return a
		Left ex -> do
			liftIO $ putStrLn $ "Caught " ++ show ex ++ ", reconnecting after 2 sec..."
			threadDelay (2000*1000)
			inGameL gameid ingamem
--	where
--	catch_only_EOF ex = if isEOFError ex then Just () else Nothing

pingG :: (MonadIO m) => InGameM m ()
pingG = do
	InGameState{..} <- get
	when (isNothing igsLastPing) $ do
		sendG $ LichessMsg (Just 0) "p" Nothing
		now <- liftIO $ getTime Monotonic
		modify $ \ s -> s { igsLastPing = Just now }

sendG :: (MonadIO m) => LichessMsg -> InGameM m ()
sendG lichessmsg = do
	InGameState{..} <- get
	liftIO $ WS.sendTextData igsConnection lichessmsg
	logMsg $ "SENT: " ++ show lichessmsg ++ "\n"

{-
receiveG :: (MonadIO m) => InGameM m LichessMsg
receiveG = do
	InGameState{..} <- get
--	liftIO $ putStrLn $ "receiveG: receiveDataMessage"
	datamsg <- liftIO $ receiveDataMessage igsConnection
	let (lichessmsg,x) :: (LichessMsg,String) = case datamsg of
		Text   x -> (fromLazyByteString x,BSL8.unpack x)
		Binary x -> (fromLazyByteString x,BSL8.unpack x)
--	liftIO $ putStrLn $ "receiveG: " ++ x
--	lichessmsg <- liftIO $ WS.receiveData igsConnection
	liftIO $ putStrLn $ "receiveG: " ++ show lichessmsg
--	logMsg $ "RECV = " ++ x ++ "\n"
	logMsg $ "RECV: " ++ show lichessmsg ++ "\n"
	return lichessmsg
-}
receiveG :: (MonadIO m) => InGameM m LichessMsg
receiveG = do
	InGameState{..} <- get
	lichessmsg <- liftIO $ WS.receiveData igsConnection
	liftIO $ putStrLn $ "receiveG: " ++ show lichessmsg
	logMsg $ "RECV: " ++ show lichessmsg ++ "\n"
	return lichessmsg

sendMoveG :: (MonadIO m) => Move -> InGameM m ()
sendMoveG move = do
	let(from,to,prom) = case move of
		Move{..} -> (moveFrom,moveTo,movePromote)
		Castling col Queenside -> ((5,baseRank col),(3,baseRank col),Nothing)
		Castling col Kingside  -> ((5,baseRank col),(7,baseRank col),Nothing)
	sendG $ LichessMsg Nothing "move" $ Just $ PMyMove $ MyMove $ MoveFromTo from to prom

waitForMoveG :: (MonadIO m) => InGameM m (Either Move MatchResult)
waitForMoveG = do
	liftIO $ putStrLn "WAITING FOR MOVE G"
	receiveG >>= handlemsg . (:[])
	where
	handlemsg [] = waitForMoveG
	handlemsg ((LichessMsg _ t mb_payload):rs) = do
		case (t,mb_payload) of

			("ack",Nothing) -> handlemsg rs
			(_,Just (PMessages msgs)) -> handlemsg $ msgs ++ rs

			(_,Just (PLiMove limove)) -> do
				pos <- gets igsPosition
				let
					MoveFromTo from to mb_promote = uci (limove::LiMove)
					move_ply = ply (limove::LiMove)
					pos_ply = pNextMoveNumber pos * 2 - 1 + if pColourToMove pos == White then 0 else 1
				case move_ply == pos_ply of
					False -> handlemsg rs
					True -> do
						liftIO $ putStrLn $ "========= GOT " ++ show (from,to)
						return $ Left $ head $ [ move | move <- moveGen pos,
							case move of
								Move{..}          -> moveFrom==from && moveTo==to && movePromote==mb_promote
								Castling col side -> col == pColourToMove pos && from==(5,baseRank col) &&
									to==(if side==Queenside then 1 else 8,baseRank col) ]

			("n",_) -> do
				now <- liftIO $ getTime Monotonic
				mb_lastping <- gets igsLastPing
				case mb_lastping of
					Just lastping -> do
						let lag_ns = toNanoSecs $ diffTimeSpec now lastping
--						liftIO $ putStrLn $ "### Lag = " ++ show (div lag_ns 1000000) ++ " ms"
						modify $ \ s -> s { igsLastPing = Nothing }
					Nothing -> return ()
				handlemsg rs

			(_,Just (PEndData (EndData winnerordraw gamestatus))) -> do
				return $ Right $ case winnerordraw of
					WDWinner colour -> Winner colour Checkmate  -- TODO: WinReason herausfinden
					WDDraw          -> Draw NoMatePossible      -- TODO: DrawReason herausfinden

			(_,Just (PCrowd crowd)) -> handlemsg rs

			(t,Just (PUnknown v)) -> do
				liftIO $ putStrLn $ "###### listenForLichessG: Could not parse t=" ++ show t ++ ": " ++ show v
				handlemsg rs

			unknown -> do
				liftIO $ putStrLn $ "###### listenForLichessG: No impl. for " ++ show unknown
				handlemsg rs
