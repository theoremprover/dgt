{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,
	TupleSections,StandaloneDeriving,ScopedTypeVariables,FlexibleContexts #-}

module Main where

--import System.Environment
import Control.Monad.IO.Class
--import Data.Char
--import Data.Bits
import Text.Printf
import System.IO
import Control.Monad (unless,forever)
import Control.Monad.Loops
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State.Strict (StateT,modify,get,gets,evalStateT)
--import Control.Monad.Trans.Class (lift)
--import Control.Monad (when)
import Control.Concurrent.Chan.Lifted

import Log
import DGTSerial
import Chess200
import Lichess
import LichessInterface
--import Confluence


data MainS = MainS {
	msMyColour       :: Colour,
	msPosition       :: Position
	}
type MainM = StateT MainS

liftDGT = lift . lift . lift
liftInGame = lift

main = withLichessDo $ do
	initLog
	
	comport <- readFile "dgtcom.txt"
	withDGT comport $ do
		pw <- liftIO $ readFile "pw.txt"
		withLoginL "Threetee" pw $ \ user -> do
			gamedata <- case nowPlaying (user::User) of
				Just (game:_) -> joinGameL (gameId game)
				_             -> startGameL Nothing (Just White)
			inGameL gamedata $ \ pos mycolour -> do			
				flip evalStateT (MainS mycolour pos) $ do
					liftIO $ print pos
					liftDGT $ displayTextDGT "Setup" True
					waitForPosOnDGT
					gameLoop

gameLoop = do
	MainS mycol pos <- get
	liftIO $ print pos

	move_or_end <- case mycol == pColourToMove pos of
		True -> do
			liftIO $ putStrLn "Waiting for DGT move..."
			move <- liftDGT $ do
				displayTextDGT "You" False
				getMoveDGT pos
			return $ Left move
		False -> do
			liftIO $ putStrLn "Waiting for Lichess move..."
			liftDGT $ displayTextDGT "Wait" False
			liftInGame $ waitForMoveG pos

	mb_matchresult <- case move_or_end of
		Left move -> do
			let pos' = doMove pos move
			modify $ \ s -> s { msPosition = pos' }

			case mycol == pColourToMove pos of
				True -> liftInGame $ do
					sendMoveG move
				False -> do
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
		iterateUntil (== pBoard pos) $ do
			liftIO $ putStrLn "waitFieldUpdateDGT..."
			waitFieldUpdateDGT
			liftIO $ putStrLn "getBoardDGT..."
			getBoardDGT
		liftIO $ putStrLn "OK"
		displayTextDGT "OK" True


{-
	lichesschan <- newChan
	(pos,mycolour) <- forkLichessThread "Threetee" pw lichesschan msgChan

	flip evalStateT (MainS msgChan lichesschan dgtchan mycolour pos) $ do
		waitForPosOnDGT
		mainLoopA

readMsg = gets msConfluenceChan >>= readChan

waitForMsg msg = iterateUntil (==msg) $ do
	liftIO $ putStrLn $ "waitForMsg " ++ show msg ++ "..."
	msgw <- readMsg
	liftIO $ print msgw
	return msgw

waitForPosOnDGT = do
	pos <- gets msPosition
	writeDGTCmd $ WaitForPos pos
	waitMsg DGTPositionIsSetup

waitForMoveDGT = do
	pos <- gets msPosition
	writeDGTCmd $ WaitForMove pos
	msg <- readMsg
	case msg of
		

writeDGTCmd cmd = do
	liftIO $ putStrLn $ "writeDGTCmd " ++ show cmd
	chan <- gets msDGTChan
	writeChan chan cmd

mainLoopA = do
	MainS{..} <- get
	case msMyColour == pColourToMove msPosition of
		True -> do
			move <- waitForMoveDGT 
			modify $ \ s -> s { msPosition = doMove msPosition move }
		False -> do
			

	liftIO $ putStrLn "End."
-}

{-
main2 = do
	serialport:args <- getArgs
	withDGT serialport $ do
		board <- getBoard
		let pos = initialPosition { pBoard = board }
		loop 2 pos

		where

		loop maxdepth pos = do
			displayTextDGT "You move" False
			liftIO $ do
				print pos
				putStrLn $ printf "Rating = %.2f" (fst $ rate pos)
				putStrLn $ "Possible moves are:" ++ show (moveGen pos)
			case rate pos of
				(_,Just matchresult) → liftIO $ print matchresult
				_ | otherwise        → do
					player_move <- getMoveDGT pos
					liftIO $ putStrLn $ "Player moving " ++ show player_move

					let pos' = doMove pos player_move
					displayTextDGT "Thinking" False
					let computer_move = last $ snd $ search True maxdepth pos' []
					liftIO $ putStrLn $ "Computer moving " ++ show computer_move
					iterateUntil (==computer_move) $ do
						displayTextDGT (show computer_move) True
						getMoveDGT pos'

					loop maxdepth (doMove pos' computer_move)
-}