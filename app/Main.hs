{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,
	TupleSections,StandaloneDeriving,ScopedTypeVariables,FlexibleContexts #-}

module Main where

--import System.Environment
import Control.Monad.IO.Class
import Text.Printf
import System.IO
import Control.Monad (unless,forever,when)
import Control.Monad.Loops
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (StateT,modify,get,gets,evalStateT)
import Control.Concurrent.Chan.Lifted
import System.Random

import Log
import DGTSerial
import Chess200
import Lichess
import LichessInterface


data MainS = MainS {
	msMyColour       :: Colour,
	msPosition       :: Position,
	msSimulDGT       :: Bool
	}
type MainM = StateT MainS

liftDGT = lift . lift . lift
liftInGame = lift

main = withLichessDo $ do
	initLog
	
	comport <- readFile "dgtcom.txt"
	let simuldgt = null comport
	( case simuldgt of
		True  -> flip evalStateT (DGTState Nothing) 
		False -> withDGT comport ) $ do
		pw <- liftIO $ readFile "pw.txt"
		withLoginL "Threetee" pw $ \ user -> do
			gamedata <- case nowPlaying (user::User) of
				Just (game:_) -> joinGameL (gameId game)
				_             -> startGameL Nothing (Just White)
			inGameL gamedata $ \ pos mycolour -> do			
				flip evalStateT (MainS mycolour pos simuldgt) $ do
					liftIO $ print pos
					when (not simuldgt) $ do
						liftDGT $ displayTextDGT "Setup" True
						waitForPosOnDGT
					gameLoop

gameLoop = do
	MainS mycol pos simuldgt <- get
	liftIO $ print pos

	move_or_end <- case mycol == pColourToMove pos of
		True -> do
			liftIO $ putStrLn "Waiting for DGT move..."
			move <- case simuldgt of
				True  -> do
					let moves = moveGen pos
					i <- liftIO $ getStdRandom (randomR (0,length moves - 1))
					return $ moves !! i
				False -> liftDGT $ do
					displayTextDGT "You" False
					getMoveDGT pos
			return $ Left move
		False -> do
			liftIO $ putStrLn "Waiting for Lichess move..."
			when (not simuldgt) $ liftDGT $ displayTextDGT "Wait" False
			liftInGame $ waitForMoveG pos

	mb_matchresult <- case move_or_end of
		Left move -> do
			let pos' = doMove pos move
			modify $ \ s -> s { msPosition = pos' }

			case mycol == pColourToMove pos of
				True -> liftInGame $ do
					sendMoveG move
				False -> do
					when (not simuldgt) $ do
						liftDGT $ displayTextDGT (show move) True
						waitForPosOnDGT

			return $ snd $ rate pos'
		Right matchresult -> return $ Just matchresult

	case mb_matchresult of
		Nothing -> gameLoop
		Just matchresult -> do
			liftIO $ print matchresult

waitForPosOnDGT :: MainM (InGameM (LichessM (DGTM IO))) ()
waitForPosOnDGT = do
	liftIO $ putStrLn "waitForPosOnDGT..."
	pos <- gets msPosition
	liftDGT $ do
		waitloop (pBoard pos)
		displayTextDGT "OK" True
	liftIO $ putStrLn "OK"
	where
	waitloop desired_board = do
		board <- getBoardDGT
		when (board /= desired_board) $ do
			waitFieldUpdateDGT
			waitloop desired_board
