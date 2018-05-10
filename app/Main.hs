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
import System.IO
import Text.Printf
import Control.Monad.Loops
import Control.Monad.Trans.State.Strict (get,gets)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)
import Control.Concurrent.Chan.Lifted

import DGTSerial
import Chess200
import Lichess
import LichessInterface


type MainChan = Chan ChanMsg
data MainChanMsg =
	LichessMsg LichessMsg |
	DGTMsg Move
	deriving (Show)

main = do
	fromChan <- newChan

	toDgtChan <- newChan
	fork $ dgtThread toDgtChan fromChan

	toLichessChan <- newChan
	fork $ lichessThread toLichessChan fromChan

lichessThread inputchan outputchan = do
	pw <- readFile "pw.txt"
	withLoginL "Threetee" pw $ \ user -> do
		case nowPlaying (user::User) of
			Just (game:_) -> joinGameL (gameId game) gameloop
			_             -> startGameL Nothing (Just White) gameloop

	where

	gameloop = do
		igs <- sharedGet
		let pos@Position{..} = igsCurrentPos igs
		liftIO $ appendFile "msgs.log" $ show igs ++ "\n"
		when ( pColourToMove == igsMyColour igs && pNextMoveNumber == igsMyNextMove igs && not (igsMyMoveSent igs)) $ do
			let move:_ = moveGen pos
			sendMoveG move
		messageLoopG
		inprogress <- sharedGets igsGameInProgress
		case inprogress of
			True  -> gameloop
			False -> liftIO $ putStrLn "GAME END."

data DGTCommand = WaitForMove Move deriving Show

dgtThread inputchan outputchan = do
	comport <- readFile "dgtcom.txt"
	board <- getBoard
	let pos = initialPosition { pBoard = board }
	withDGT comport $ do
		fork $ listen_main
		listen_dgt pos

	where

	listen_main = do
		command <- readChan inputchan
		case command of
			WaitForMove move -> 

	listen_dgt pos = do
		move <- getMoveDGT pos
		writeChan outputchan $ DGTMsg move
		liftIO $ putStrLn $ "dgtThread writeChan " ++ show move
		listen_dgt $ doMove pos move

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