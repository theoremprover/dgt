{-# LANGUAGE OverloadedStrings,DuplicateRecordFields,DeriveGeneric,RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
https://github.com/ornicar/lila/blob/master/doc/mobile/play.md
https://github.com/veloce/lichobile/tree/master/src
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


socketVersion = "2"

data NoResponse = NoResponse deriving (Show)
instance FromJSON NoResponse where
	parseJSON _ = pure NoResponse


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
	name        :: Maybe String,
	ai          :: Maybe Int,
	color       :: Maybe Colour,
	user        :: Maybe User,
	rating      :: Maybe Int,
	provisional :: Maybe Bool,
	version     :: Maybe Int,
	spectator   :: Maybe Bool } deriving (Show,Generic)
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
	createdAt     :: MyUTCTime,
	winner        :: Maybe String,
	lastMove      :: Maybe String,
	opening       :: Maybe Opening
	} deriving (Show,Generic)
instance FromJSON CreatedGame

data Opening = Opening {
	eco  :: String,
	name :: String,
	ply  :: Int } deriving (Show,Generic)
instance FromJSON Opening

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
	coords            :: Int,
	replay            :: Int,
	autoQueen         :: Maybe Int,
	clockTenths       :: Int,
	moveEvent         :: Maybe Int,
	clockBar          :: Bool,
	clockSound        :: Maybe Bool,
	rookCastle        :: Bool,
	highlight         :: Bool,
	destination       :: Bool,
	showCaptured      :: Bool } deriving (Show,Generic)
instance FromJSON Pref

data GameStep = GameStep {
	ply :: Depth,
	fen :: FEN,
	san :: Maybe String,
	uci :: Maybe String } deriving (Show,Generic)
instance FromJSON GameStep

data GameData = GameData {
	game           :: CreatedGame,
	clock          :: Maybe String,
	correspondence :: Maybe String,
	player         :: Player,
	opponent       :: Player,
	orientation    :: Maybe String,
	url            :: LiURL,
	pref           :: Pref,
	evalPut        :: Maybe Bool,
	takebackable   :: Maybe Bool,
	possibleMoves  :: Maybe PossibleMoves,
	steps          :: [GameStep],
	chat           :: Maybe [String] } deriving (Show,Generic)
instance FromJSON GameData

data PossibleMoves = PossibleMoves [(String,String)] deriving Show
instance FromJSON PossibleMoves where
	parseJSON = withObject "PossibleMoves" $ \ v -> PossibleMoves <$> parseObjectToAssocList v

data LichessMsg d = LichessMsg {
	t :: String,
	d :: Maybe d } deriving (Show,Generic)
instance (ToJSON d) => ToJSON (LichessMsg d)
instance (FromJSON d) => FromJSON (LichessMsg d)

data LiMove = LiMove {
	u :: String } deriving (Show,Generic)
instance FromJSON LiMove
instance ToJSON LiMove
