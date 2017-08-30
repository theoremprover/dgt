{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,
	TupleSections,StandaloneDeriving,ScopedTypeVariables #-}

module Main where

{-
stack build
stack exec dgt-exe "COM13"
stack ghci
-}

import System.Environment
import Control.Monad.IO.Class
import Data.Char
import Data.Bits
import Text.Printf

import DGTSerial
import ChessLib

main = do
	serialport:args <- getArgs
	withDGT serialport $ do
		board <- getBoard
		let pos = initialPosition { pBoard = board }
		loop pos

		where
		loop pos = do
			liftIO $ print pos
			move <- getMoveDGT pos
			loop $ doMove pos move
