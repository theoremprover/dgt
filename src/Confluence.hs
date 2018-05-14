{-# OPTIONS_GHC -fno-warn-tabs #-}

module Confluence (
	module Confluence,
	module Control.Concurrent.Chan.Lifted
	) where

import Control.Concurrent.Chan.Lifted

import LichessInterface
import Chess200

type ConfluenceChan = Chan ConfluenceMsg

data ConfluenceMsg =
	LichessGameStart Position |
	LichessGameEnd MatchResult |
	LichessMove MoveFromTo |
	DGTMoveDone |
	DGTPositionIsSetup |
	DGTMove Move
	deriving (Show)
