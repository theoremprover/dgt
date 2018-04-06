{-# LANGUAGE OverloadedStrings,RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module LichessWebsockets where

import Network.Connection
import Wuss
import Network.Socket (withSocketsDo)
import Network.TLS
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BS
import Network.WebSockets as WS
import Network.WebSockets.Stream
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State
import System.Random

import Chess200
import Lichess

type InGameM a = StateT InGameState LichessM a

data InGameState = InGameState {
	igsConnection :: Connection } deriving Show

inGameL :: ImGameM a -> LichessM a
inGameL ingamem = do
	clientID <- forM [1..10] $ \ _ -> getStdRandom (randomR ('a','z'))
	Just socketurl <- gets socketURL
	let baseurl = socketurl ++ "?sri=" ++ clientID ++ "&version=" ++ socketVersion
	withSocketsDo $ do
		let (host,port,options,headers) = ("socket.lichess.org",9021,defaultConnectionOptions,[]) 
		context <- initConnectionContext
		connection <- connectTo context $ ConnectionParams {
			connectionHostname = host, connectionPort = port,
			connectionUseSecure = Just $ TLSSettingsSimple {
				settingDisableCertificateValidation = True,
				settingDisableSession = False,
				settingUseServerName = False },
			connectionUseSocks = Nothing }
		stream <- makeStream
			(fmap Just (connectionGetChunk connection))
			(maybe (return ()) (connectionPut connection . BS.toStrict))
		WS.runClientWithStream stream host baseurl options headers $ \ conn -> do
			evalStateT ingamem $ InGameState conn



sendG :: String -> String 
sendG
{-
			WS.sendTextData conn ("TESTLINE!" :: BS.ByteString)
			WS.receiveData conn >>= BS.putStrLn
-}

moveG :: String -> String -> LichessM ()
moveG from to = do
	return ()
