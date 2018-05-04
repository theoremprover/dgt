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
import Control.Monad.Trans.State.Strict (get,modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)

import DGTSerial
import Chess200
import Lichess
import LichessInterface

main = do
	pw <- readFile "pw.txt"
	withLoginL "Threetee" pw $ \ user -> do
		case nowPlaying (user::User) of
			Just (game:_) -> joinGameL (gameId game) gameloop
			_             -> startGameL Nothing (Just White) gameloop

gameloop = do
	InGameState{..} <- get
	when (pColourToMove igsCurrentPos == igsMyColour) $ do
		let move:_ = moveGen igsCurrentPos
		sendMoveG move
	messageloop
	gameloop

messageloop = do
	LichessMsg _ t mb_payload <- receiveG
--	liftIO $ print lichessmsg
	case (t,mb_payload) of
		("ack",Nothing) -> messageloop
		(_,Just (POpponentMove opponentmove)) -> do
			modify $ \ s -> s { igsCurrentPos = doMove (igsCurrentPos s) (opponentMove2Move (igsCurrentPos s) opponentmove) }
			return ()
		_ -> messageloop

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
