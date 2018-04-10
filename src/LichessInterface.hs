{-# LANGUAGE OverloadedStrings,DuplicateRecordFields,DeriveGeneric,RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
https://github.com/ornicar/lila/blob/master/doc/mobile/play.md
-}

module LichessInterface where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Monad
import GHC.Generics

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Chess200


socketVersion = "3"

data Profile = Profile {
	country   :: Maybe String,
	firstName :: Maybe String } deriving (Show,Generic)
instance FromJSON Profile

parseObjectToAssocList v = forM (HM.toList v) $ \ (t,p) -> do
	pp <- parseJSON p
	return (T.unpack t,pp)

data Perf = Perf {
	games  :: Int,
	rating :: Int,
	rd     :: Int,
	prog   :: Int,
	prov   :: Maybe Bool } deriving (Show,Generic)
instance FromJSON Perf

data Perfs = Perfs [(String,Perf)] deriving Show
instance FromJSON Perfs where
	parseJSON = withObject "Perfs" $ \ v -> Perfs <$> parseObjectToAssocList v

data PlayTime = PlayTime {
	total :: POSIXTime,
	tv    :: POSIXTime } deriving (Show,Generic)
instance FromJSON PlayTime

newtype MyUTCTime = MyUTCTime UTCTime
instance Show MyUTCTime where
	show (MyUTCTime utctime) = show utctime
instance FromJSON MyUTCTime where
	parseJSON = withScientific "MyUTCTime" $ \ n -> pure $ MyUTCTime $ posixSecondsToUTCTime (realToFrac n / 1000)

data User = User {
	id         :: String,
	username   :: String,
	online     :: Bool,
	perfs      :: Perfs,
	createdAt  :: Maybe MyUTCTime,
	profile    :: Profile,
	seenAt     :: Maybe MyUTCTime,
	playTime   :: Maybe PlayTime,
	language   :: String,
	nowPlaying :: Maybe [Game] } deriving (Show,Generic)
instance FromJSON User

data Variant = Variant {
	key :: String,
	name :: String } deriving (Show,Generic)
instance FromJSON Variant

data Player = Player {
	id          :: Maybe String,
	username    :: Maybe String,
	ai          :: Maybe Int,
	color       :: Maybe Colour,
	user        :: Maybe User,
	rating      :: Maybe Int,
	provisional :: Maybe Bool,
	version     :: Maybe Int } deriving (Show,Generic)
instance FromJSON Player

data Game = Game {
	fullId      :: String,
	gameId      :: String,
	fen         :: String,
	color       :: Colour,
	lastMove    :: String,
	variant     :: Variant,
	speed       :: String,
	perf        :: String,
	rated       :: Bool,
	opponent    :: Player,
	isMyTurn    :: Bool,
	secondsLeft :: Maybe Int } deriving (Show,Generic)
instance FromJSON Game

instance FromJSON Colour where
	parseJSON (String c) = pure $ case c of
		"white" -> White
		"black" -> Black

type FEN = String

data CreatedGame = CreatedGame {
	id            :: String,
	variant       :: Variant,
	speed         :: String,
	perf          :: String,
	rated         :: Bool,
	initialFen    :: FEN,
	fen           :: FEN,
	player        :: Colour,
	turns         :: Depth,
	startedAtTurn :: Depth,
	source        :: String,
	status        :: GameStatus,
	createdAt     :: MyUTCTime } deriving (Show,Generic)
instance FromJSON CreatedGame

data GameStatus = GameStatus {
	id   :: Int,
	name :: String } deriving (Show,Generic)
instance FromJSON GameStatus

data LiURL = LiURL {
	socket :: String,
	round  :: String } deriving (Show,Generic)
instance FromJSON LiURL

data Pref = Pref {
	animationDuration :: Int,
	coords       :: Int,
	replay       :: Int,
	autoQueen    :: Int,
	clockTenths  :: Int,
	moveEvent    :: Int,
	clockBar     :: Bool,
	clockSound   :: Bool,
	rookCastle   :: Bool,
	highlight    :: Bool,
	destination  :: Bool,
	showCaptured :: Bool } deriving (Show,Generic)
instance FromJSON Pref

data GameStep = GameStep {
	ply :: Depth,
	fen :: FEN,
	san :: Maybe String,
	uci :: Maybe String } deriving (Show,Generic)
instance FromJSON GameStep

data GameData = GameData {
	game          :: CreatedGame,
	player        :: Player,
	opponent      :: Player,
	url           :: LiURL,
	pref          :: Pref,
	takebackable  :: Bool,
	possibleMoves :: PossibleMoves,
	steps         :: [GameStep] } deriving (Show,Generic)
instance FromJSON GameData

data PossibleMoves = PossibleMoves [(String,String)] deriving Show
instance FromJSON PossibleMoves where
	parseJSON = withObject "PossibleMoves" $ \ v -> PossibleMoves <$> parseObjectToAssocList v

data LichessMsg d = LichessMsg {
	t :: String,
	d :: Maybe d } deriving (Show,Generic)
instance (ToJSON d) => ToJSON (LichessMsg d)

data LiMove = LiMove {
	from      :: String,
	to        :: String,
	promotion :: Maybe String } deriving (Show,Generic)
instance FromJSON LiMove
instance ToJSON LiMove

{-
instance ToJSON Move where
	toJSON Move{..} = object $ [
		"from" .= show moveFrom,
		"to"   .= show moveTo ] ++
		maybe [] ((:[]) . ("promotion" .=) . toPieceName) movePromote
		where
		toPieceName :: Piece -> String
		toPieceName Ú = "knight"
		toPieceName Û = "bishop"
		toPieceName Ü = "rook"
		toPieceName Ý = "queen"
-}

{-
export interface MoveOrDrop {
  readonly fen: string
  readonly threefold: boolean
  readonly check: boolean
  readonly ply: number
  readonly wDraw: boolean
  readonly bDraw: boolean
  readonly uci: string
  readonly san: string
  readonly dests: StringMap
  readonly status?: GameStatus
  readonly winner?: Color
  readonly crazyhouse?: {
    readonly pockets: Pockets
  }
  clock?: {
    readonly white: number
    readonly black: number
    readonly lag?: number
  }
  readonly promotion?: {
    readonly key: Key
    readonly pieceClass: Role
  }
  readonly enpassant?: {
    readonly key: Key
    readonly color: Color
  }
  readonly drops?: Array<string>
  readonly castle?: {
    readonly king: KeyPair
    readonly rook: KeyPair
    readonly color: Color
  }
}

export interface Move extends MoveOrDrop {
  isMove: boolean
}

----------------------



-}

