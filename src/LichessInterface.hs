{-# LANGUAGE OverloadedStrings,DuplicateRecordFields,DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module LichessInterface (
	module Data.Aeson,
	User(..),GameData(..)
	) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Monad
import GHC.Generics

import Data.Time.Clock
import Data.Time.Clock.POSIX

import Chess200


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
	status        :: Status,
	createdAt     :: MyUTCTime } deriving (Show,Generic)
instance FromJSON CreatedGame

data Status = Status {
	id   :: Int,
	name :: String } deriving (Show,Generic)
instance FromJSON Status

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

{-
rawLichessRequest response body: {
"game":{
	"id":"cNoq5g57",
	"variant":{
		"key":"standard","name":"Standard","short":"Std"},
	"speed":"correspondence",
	"perf":"correspondence",
	"rated":false,
	"initialFen":"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
	"fen":"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
	"player":"white",
	"turns":0,
	"startedAtTurn":0,
	"source":"ai",
	"status":{"id":20,"name":"started"},
	"createdAt":1522182914716
	},
"player":{
	"color":"white",
	"user":{
		"id":"threetee",
		"username":"Threetee",
		"online":true,
		"perfs":{
			"correspondence":{
				"games":16,"rating":1608,"rd":133,"prog":127,"prov":true }},
		"language":"de-DE",
		"profile":{"country":"DE"}},
	"rating":1608,
	"provisional":true,
	"id":"uhAS",
	"version":0},
"opponent":{
	"color":"black",
	"ai":2,
	"onGame":true},
"url":{
	"socket":"/cNoq5g57uhAS/socket/v3",
	"round":"/cNoq5g57uhAS"},
"pref":{
	"animationDuration":300,
	"coords":2,
	"replay":2,
	"autoQueen":2,
	"clockTenths":1,
	"moveEvent":2,
	"clockBar":true,
	"clockSound":true,
	"rookCastle":true,
	"highlight":true,
	"destination":true,
	"showCaptured":true},
"takebackable":true,
"possibleMoves":{"b2":"b3b4","g2":"g3g4","c2":"c3c4","b1":"a3c3","g1":"f3h3","h2":"h3h4","d2":"d3d4","e2":"e3e4","a2":"a3a4","f2":"f3f4"},
"steps":[
	{"ply":0,"uci":null,"san":null,"fen":"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"}]


----------------------


rawLichessRequest response body: {
"id":"threetee",
"username":"Threetee",
"online":false,
"perfs":{"blitz":{"games":2035,"rating":1467,"rd":60,"prog":-40},"bullet":{"games":747,"rating":1366,
"rd":60,"prog":-23},"correspondence":{"games":16,"rating":1608,"rd":133,"prog":127,"prov":true
},"puzzle":{"games":2430,"rating":1636,"rd":61,"prog":101},"classical":{"games":5,"rating":165
4,"rd":150,"prog":0,"prov":true},"rapid":{"games":255,"rating":1654,"rd":63,"prog":0}},"create
dAt":1449173808339,"profile":{"country":"DE","firstName":"Robert"},"seenAt":1522267844474,"pla
yTime":{"total":1505994,"tv":0},"language":"de-DE","nowPlaying":[{"fullId":"JVPvX1BxWW1y","gam
eId":"JVPvX1Bx","fen":"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR","color":"white","lastMove"
:"","variant":{"key":"standard","name":"Standard"},"speed":"correspondence","perf":"correspond
ence","rated":false,"opponent":{"id":null,"username":"A.I. level 2","ai":2},"isMyTurn":true,"s
econdsLeft":null},{"fullId":"UZyB33TabQ9C","gameId":"UZyB33Ta","fen":"rnbqkbnr/pppppppp/8/8/8/
8/PPPPPPPP/RNBQKBNR","color":"white","lastMove":"","variant":{"key":"standard","name":"Standar
d"},"speed":"correspondence","perf":"correspondence","rated":false,"opponent":{"id":null,"user
name":"A.I. level 2","ai":2},"isMyTurn":true,"secondsLeft":null},{"fullId":"5BFmVPoMiTVe","gam
eId":"5BFmVPoM","fen":"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR","color":"white","lastMove"
:"","variant":{"key":"standard","name":"Standard"},"speed":"correspondence","perf":"correspond
ence","rated":false,
"opponent":{"id":null,"username":"A.I. level 2","ai":2},"isMyTurn":true,"s
econdsLeft":null},{"fullId":"x0UtnMItCjio","gameId":"x0UtnMIt","fen":"rnbqkbnr/pppppppp/8/8/8/
8/PPPPPPPP/RNBQKBNR","color":"white","lastMove":"","variant":{"key":"standard","name":"Standar
d"},"speed":"correspondence","perf":"correspondence","rated":false,"opponent":{"id":null,"user
name":"A.I. level 2","ai":2},"isMyTurn":true,"secondsLeft":null}]}
dgt-exe.EXE: rawLichessRequest eitherDecodeStrict: Error in $.nowPlaying[0].opponent: key "col
or" not present
}


-}

