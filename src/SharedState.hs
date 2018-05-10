{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module SharedState (
	module SharedState,
	module Control.Monad.Trans.State.Strict
	) where

import Control.Concurrent.MVar.Lifted
import Control.Monad.Trans.State.Strict
import Control.Monad.Base
import Control.Monad.Trans.Control


type SharedState = MVar
type SharedStateT s m = StateT (SharedState s) m

sharedGet :: (MonadBase IO m) => StateT (SharedState s) m s
sharedGet = do
	mvar <- get
	readMVar mvar

sharedGets :: (MonadBase IO m) => (s -> a) -> SharedStateT s m a
sharedGets selector = sharedGet >>= return . selector

sharedModify :: (MonadBaseControl IO m) => (s -> s) -> SharedStateT s m ()
sharedModify f = do
	mvar <- get
	modifyMVar_ mvar (return . f)

evalSharedStateT :: (MonadBase IO m) => SharedStateT s m a -> s -> m a
evalSharedStateT m s = newMVar s >>= evalStateT m
