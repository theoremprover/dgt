{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module SharedState (
	module SharedState,
	module Control.Monad.Trans.Reader
	) where

import Control.Concurrent.MVar.Lifted
import Control.Monad.Trans.Reader
import Control.Monad.Base
import Control.Monad.Trans.Control

type SharedStateT s = ReaderT (MVar s)

sharedGet :: (MonadBase IO m) => SharedStateT s m s
sharedGet = ask >>= readMVar

sharedGets :: (MonadBase IO m) => (s -> a) -> SharedStateT s m a
sharedGets selector = sharedGet >>= return . selector

sharedModify :: (MonadBaseControl IO m) => (s -> s) -> SharedStateT s m ()
sharedModify f = withSharedState (return . f)

withSharedState :: (MonadBaseControl IO m) => (s -> SharedStateT s m s) -> SharedStateT s m ()
withSharedState m = do
	mvar <- ask
	modifyMVar_ mvar m

evalSharedStateT :: (MonadBase IO m) => SharedStateT s m a -> s -> m a
evalSharedStateT m s = newMVar s >>= runReaderT m
