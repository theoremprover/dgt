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
import Control.Monad.Trans.State.Strict (StateT,modify,gets,evalStateT)
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
					waitForPosOnDGT
{-
					board <- lift $ lift $ lift $ getBoardDGT
					modify $ \ s -> s { msPosition = (msPosition s) { pBoard = board } }
					forever $ do
						pos <- gets msPosition
						move <- lift $ lift $ lift $ getMoveDGT pos
						modify $ \ s -> s { msPosition = doMove (msPosition s) move }
						pos <- gets msPosition
						liftIO $ print pos
-}

waitForPosOnDGT :: MainM (InGameM (LichessM (DGTM IO))) ()
waitForPosOnDGT = do
	liftIO $ putStrLn "waitForPosOnDGT..."
	pos <- gets msPosition
	lift $ lift $ lift $ do
		displayTextDGT "Setup" True
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