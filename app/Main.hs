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


liftDGT = lift . lift . lift
liftInGame = lift

data MainS = MainS {
	msSimulDGT :: Bool
	}
type MainM = StateT MainS

main = withLichessDo $ do
	initLog
	
	comport <- readFile "dgtcom.txt"
	let simuldgt = null comport
	( case simuldgt of
		True  -> flip evalStateT (DGTState Nothing) 
		False -> withDGT comport ) $ do
		pw <- liftIO $ readFile "pw.txt"
		withLoginL "Threetee" pw $ \ user -> do
			gameid <- case nowPlaying (user::User) of
				Just (game:_) -> return (gameId game)
				_             -> startGameL Nothing (Just White)
			
			inGameL gameid $ do			
				flip evalStateT (MainS simuldgt) $ do
					pos <- liftInGame $ gets igsPosition
					liftIO $ print pos
					whenNotSimulDGT $ do
						liftDGT $ displayTextDGT "Setup" True
						waitForPosOnDGT
					gameLoop

gameLoop = do
	InGameState{..} <- liftInGame $ get
	liftIO $ print igsPosition

	move_or_end <- case igsMyColour == pColourToMove igsPosition of
		True -> do
			liftIO $ putStrLn "Waiting for DGT move..."
			simuldgt <- gets msSimulDGT
			move <- case simuldgt of
				True  -> do
					let moves = moveGen igsPosition
					i <- liftIO $ getStdRandom (randomR (0,length moves - 1))
					return $ moves !! i
				False -> liftDGT $ do
					displayTextDGT "You" False
					getMoveDGT igsPosition
			return $ Left move
		False -> do
			liftIO $ putStrLn "Waiting for Lichess move..."
			whenNotSimulDGT $ liftDGT $ displayTextDGT "Wait" False
			liftInGame $ waitForMoveG

	mb_matchresult <- case move_or_end of
		Left move -> do
			let pos' = doMove igsPosition move
			liftInGame $ modify $ \ s -> s { igsPosition = pos' }

			case igsMyColour == pColourToMove igsPosition of
				True  -> liftInGame $ sendMoveG move
				False -> whenNotSimulDGT $ do
					liftDGT $ displayTextDGT (show move) True
					waitForPosOnDGT

			return $ snd $ rate pos'
		Right matchresult -> return $ Just matchresult

	case mb_matchresult of
		Nothing -> gameLoop
		Just matchresult -> do
			liftIO $ print matchresult

whenNotSimulDGT m = do
	simuldgt <- gets msSimulDGT
	when (not simuldgt) m

waitForPosOnDGT :: MainM (InGameM (LichessM (DGTM IO))) ()
waitForPosOnDGT = do
	liftIO $ putStrLn "waitForPosOnDGT..."
	pos <- liftInGame $ gets igsPosition
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
