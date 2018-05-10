{-# OPTIONS_GHC -fno-warn-tabs #-}

module SharedState (
	module SharedState,
	module Control.Monad.Trans.State.Strict
	) where

import Control.Concurrent.MVar.Lifted
import Control.Monad.Trans.State.Strict


type SharedState s = MVar s
type SharedStateT s m a = StateT (SharedState s) m a

sharedGet :: Monad m => StateT s m s
sharedGet = do
	mvar <- get
	readMVar mvar

sharedGets :: Monad m => (s -> a) -> StateT s m a
sharedGets selector = sharedGet >>= return . selector

sharedModify :: Monad m => (s -> s) -> StateT s m ()
sharedModify f = do
	mvar <- get
	modifyMVar_ mvar (return . f)

evalSharedStateT :: MonadIO m => StateT (SharedState s) m a -> s -> m a
evalSharedStateT m s = newMVar s >>= evalStateT m
