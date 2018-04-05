{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module LichessWebsockets where

import Network.Socket (withSocketsDo)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State

import Chess200

type GameData = String

data LichessState = LichessState {
	myColour      :: Colour,
	currentGameID :: Maybe String } deriving Show

type LichessM m a = StateT LichessState m a

withLoginL :: (MonadIO m) => String -> String -> LichessM m a -> m a
withLoginL username password lichessm = liftIO $ do
	withSocketsDo $ WS.runClient "echo.websocket.org" 80 "/" $ \ conn -> do
		WS.sendTextData conn "TESTLINE!"
		WS.receiveData conn >>= T.putStrLn
		evalStateT lichessm $ LichessState {
			myColour = White,
			currentGameID = Nothing }

startGameL :: (MonadIO m) => Maybe Position -> Maybe Colour -> LichessM m (Maybe GameData)
startGameL mb_position mb_colour = do
	return $ Just "SOMEID"

moveL :: (MonadIO m) => String -> String -> LichessM m ()
moveL from to = do
	return ()
