{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module TunnelState where

import Control.Monad.Trans.State.Strict (StateT,get,gets,evalStateT)
import Control.Monad.IO.Class


class HasTunnelState hoststate tunnelstate where
	setTunnelState  :: tunnelstate -> hoststate
	withTunnelState :: 

runTunnelState