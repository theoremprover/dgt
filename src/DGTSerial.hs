{-# LANGUAGE RecordWildCards,TupleSections,FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module DGTSerial where

import qualified Data.ByteString.Char8 as BS
import System.Hardware.Serialport
import System.Timeout
import Control.Monad
import Control.Monad.IO.Class
import Text.Printf
import Data.Char
import Data.Array
import Data.Bits
import qualified Data.Set as Set
import Control.Monad.Loops

import Confluence
import SharedState
import Chess200


serialportSettings = SerialPortSettings CS9600 8 One NoParity NoFlowControl 1

dGT_SEND_RESET = '\x40'
dGT_SEND_BRD = '\x42'
dGT_SEND_UPDATE = '\x43'
dGT_SEND_UPDATE_BRD = '\x44'
dGT_SEND_UPDATE_NICE = '\x4b'
dGT_RETURN_SERIALNR = '\x45'
dGT_RETURN_LONG_SERIALNR = '\x55'
dGT_SEND_BATTERY_STATUS = '\x4C'
dGT_SEND_VERSION = '\x4D'

dGT_FONE = '\x00'
dGT_BOARD_DUMP = '\x06'
dGT_BWTIME = '\x0D'
dGT_FIELD_UPDATE = '\x0E'
dGT_EE_MOVES = '\x0F'
dGT_BUSADRES = '\x10'
dGT_SERIALNR = '\x11'
dGT_LONG_SERIALNR = '\x22'
dGT_TRADEMARK = '\x12'
dGT_VERSION = '\x13'

dGT_CLOCK_MESSAGE = '\x2b'
dGT_CLOCK_START_MESSAGE = 0x03
dGT_CLOCK_END_MESSAGE = 0x00
dGT_CLOCK_DISPLAY = 0x01
dGT_CLOCK_END = '\x03'
dGT_CLOCK_SETNRUN = 0x0a
dGT_CLOCK_BEEP = 0x0b
dGT_CLOCK_ASCII = 0x0c
dGT_CLOCK_SEND_VERSION = 0x09

fromBCD bcd = (shift bcd (-4)) * 10 + (bcd .&. 0x0f)

dGT2Square = [
	(1,(White,Ù)),(2,(White,Ü)),(3,(White,Ú)),( 4,(White,Û)),( 5,(White,Þ)),( 6,(White,Ý)),
	(7,(Black,Ù)),(8,(Black,Ü)),(9,(Black,Ú)),(10,(Black,Û)),(11,(Black,Þ)),(12,(Black,Ý)) ]

lookupDGT2Square c = lookup c dGT2Square

type DGTM = SharedStateT SerialPort IO

sendDGT :: Char -> [Int] -> DGTM ()
sendDGT msg_id msg = do
	s <- sharedGet
	n <- liftIO $ send s (BS.pack $ msg_id : map chr msg)
	when (length msg + 1 /= n) $ error $ printf "Sending %s" (show msg)
	return ()

recvDGT time_out = do
	s <- sharedGet
	mb_header_bs <- liftIO $ System.Timeout.timeout time_out $ rec_part s 3
	case mb_header_bs of
		Nothing -> return Nothing
		Just header_bs -> do
			let [msg_id,len_hi,len_lo] = map ord $ BS.unpack header_bs
			let payload_length = (shift len_hi 7 .|. len_lo) - 3
			msg_bs <- liftIO $ rec_part s payload_length
			return $ Just (chr $ msg_id .&. 0x7f,map ord $ BS.unpack msg_bs)
	where
	rec_part s rest = do
		msg_part <- recv s rest
		case BS.length msg_part < rest of
			False -> return msg_part
			True  -> do
				msg_rest <- rec_part s (rest - BS.length msg_part)
				return $ BS.append msg_part msg_rest

withDGT serialport proc = withSerial serialport serialportSettings $ 
	evalSharedStateT (sendDGT dGT_SEND_RESET [] >> proc)

getBoard :: DGTM Board
getBoard = do
	sendDGT dGT_SEND_BRD []
	rec_loop
	where
	rec_loop = do
		Just msg <- recvParseMsgDGT (-1)
		case msg of
			CurrentBoard board -> return board
			_ -> rec_loop

displayTextDGT :: String -> Bool -> DGTM ()
displayTextDGT text beep = do
	let msg = dGT_CLOCK_START_MESSAGE : dGT_CLOCK_ASCII :
		map ord (take 8 $ text ++ repeat ' ') ++ [0,if beep then 3 else 1,dGT_CLOCK_END_MESSAGE]
	sendDGT dGT_CLOCK_MESSAGE (length msg : msg)

displayClockDGT = do
	sendDGT dGT_CLOCK_END []

data TimeLeft = TimeLeft Int Int Int
instance Show TimeLeft where
	show (TimeLeft h m s) = printf "%02i:%02i:%02i" h m s

data DGTMessage =
	FieldUpdate Coors Square |
	CurrentBoard Board |
	ClockAck Int Int Int Int Int Int Int |
	NoClock |
	ClockTimes TimeLeft TimeLeft |
	OtherMsg Char [Int]
	deriving Show

recvParseMsgDGT time_out = do
	mb_msg <- recvDGT time_out
	case mb_msg of
		Nothing -> return Nothing
		Just (msg_id,msg) -> return $ Just $ case msg of
			[filerank,piece] | msg_id==dGT_FIELD_UPDATE ->
				FieldUpdate (toEnum (7 - mod filerank 8 + 1),div filerank 8 + 1) (lookupDGT2Square piece)
			[t0,t1,t2,t3,t4,t5,t6] | msg_id==dGT_BWTIME -> case t3 .&. 0x0f == 0x0a || t6 .&. 0x0f == 0x0a of
				True  -> ClockAck t0 t1 t2 t3 t4 t5 t6
				False | t6 .&. 1 == 0 -> NoClock
				False -> do
					let [hb,mb,sb,hw,mw,sw] = map fromBCD [t0,t1,t2,t3,t4,t5]
					ClockTimes (TimeLeft hw mw sw) (TimeLeft hb mb sb)
			squares | msg_id==dGT_BOARD_DUMP -> CurrentBoard $ array ((1,1),(8,8)) $
				zip [ (f,r) | r <- [1..8], f <- [8,7 .. 1] ] $ map lookupDGT2Square msg
			_ -> OtherMsg msg_id msg

waitFieldUpdateDGT = do
	Just msg <- recvParseMsgDGT (-1)
	case msg of
		FieldUpdate coors square -> return (coors,square)
		_ -> waitFieldUpdateDGT

getMoveDGT :: Position -> DGTM Move
getMoveDGT pos = do
	sendDGT dGT_SEND_UPDATE_BRD []
	loop $ pBoard pos
	where
	legal_moves = moveGen pos
	lookup_move = zip (map (pBoard . doMove pos) legal_moves) legal_moves
	loop board = do
		(coors,square) <- waitFieldUpdateDGT
--		liftIO $ print msg
		let board' = board // [(coors,square)]
		case lookup board' lookup_move of
			Nothing   -> loop board'
			Just move -> do
--				liftIO $ putStrLn "Waiting..."
				mb_msg' <- recvParseMsgDGT (500 * 1000)  -- Wait 500ms if move continues...
				case mb_msg' of
					Nothing -> return move
					Just (FieldUpdate coors square) -> loop $ board' // [(coors,square)]
					_ -> loop board'

data DGTCommand =
	WaitForThisMoveDone Position Move |
	WaitForPos Position |
	WaitForMove Position
	deriving Show

listenForCommandsDGT inputchan outputchan = forever $ do
	command <- readChan inputchan
	msg <- case command of
		WaitForThisMoveDone pos move -> do
			iterateUntil (==move) $ do
				displayTextDGT (show move) True
				getMoveDGT pos
			return DGTMoveDone
		WaitForPos pos -> do
			iterateUntil (== pBoard pos) $ do
				displayTextDGT "Setup" True
				waitFieldUpdateDGT
				getBoard
			return DGTPositionIsSetup
		WaitForMove pos -> do
			move <- getMoveDGT pos
			return $ DGTMove move
	writeChan outputchan msg
	liftIO $ putStrLn $ "listenForCommandsDGT: " ++ show msg
