{-# LANGUAGE UnicodeSyntax,OverloadedStrings,RecordWildCards,FlexibleInstances,UndecidableInstances,IncoherentInstances #-}
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
import Control.Monad.Trans.State.Strict
import System.Random
import Data.Aeson

import Chess200
import FEN
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
	liftIO $ putStrLn baseurl
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
	conn <- liftIO $ WS.runClientWithStream stream host baseurl options headers return
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

