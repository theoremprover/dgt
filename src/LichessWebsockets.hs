{-# LANGUAGE OverloadedStrings,RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module LichessWebsockets where

import Network.Connection
import Wuss
import Network.TLS
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import Network.WebSockets as WS
import Network.WebSockets.Stream
import Control.Monad
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State
import System.Random

import Chess200
import Lichess
import LichessInterface

type InGameM a = StateT InGameState (StateT LichessState IO) a

data InGameState = InGameState {
	igsConnection :: WS.Connection }

inGameL :: InGameM a -> LichessM a
inGameL ingamem = do
	clientID <- forM [1..10] $ \ _ -> liftIO $ getStdRandom (randomR ('a','z'))
	Just socketurl <- gets socketURL
	let baseurl = socketurl ++ "?sri=" ++ clientID ++ "&version=" ++ socketVersion
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
		(maybe (return ()) (connectionPut connection . BS.toStrict))
	liftIO $ WS.runClientWithStream stream host baseurl options headers $ \ conn -> do
		evalStateT ingamem $ InGameState conn

{-
sendG :: String -> String 
sendG
			WS.sendTextData conn ("TESTLINE!" :: BS.ByteString)
			WS.receiveData conn >>= BS.putStrLn
-}

moveG :: Move -> InGameM ()
moveG move = do
	conn <- gets igsConnection
	WS.sendTextData conn move
