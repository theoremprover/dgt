{-# LANGUAGE OverloadedStrings,LambdaCase,DuplicateRecordFields,DeriveGeneric,RecordWildCards,FlexibleContexts,FlexibleInstances,UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

{-
https://github.com/ornicar/lila/blob/master/doc/mobile/play.md
https://github.com/veloce/lichobile/tree/master/src
-}

module LichessInterface where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Text as T
import Control.Monad
import GHC.Generics
import Network.WebSockets
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict as HM

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

data LichessMsg = LichessMsg {
	v :: Maybe Int,
	t :: String,
	d :: Maybe LichessMsgPayload } deriving (Show,Generic)
instance ToJSON   LichessMsg 
instance FromJSON LichessMsg where
	parseJSON = withObject "LichessMsg" $ \ o -> LichessMsg <$>
		o .:? "v" <*>
		o .:  "t" <*>
		explicitParseFieldMaybe (parse_payload (HM.lookup "t" o)) o "d"

parse_payload :: Maybe Value -> (Value -> Parser LichessMsgPayload)
parse_payload (Just (String payload_type)) = case payload_type of
	"move"  -> withObject "POpponentMove" $ \ o -> POpponentMove <$> parseJSON (Object o)
	"b"     -> withArray "PGameStatus" $ \ a -> PGameStatus <$> parseJSON (Array a)
	"crowd" -> withObject "PCrowd" $ \ o -> PCrowd <$> parseJSON (Object o)
	unknown -> \ v -> fail $ "parse_payload: t= " ++ show unknown ++ " not implemented for " ++ show v

data LichessMsgPayload =
	POpponentMove OpponentMove |
	PMyMove MyMove |
	PGameStatus [LichessMsg] |
	PCrowd Crowd
	deriving (Show,Generic)
instance ToJSON   LichessMsgPayload where
	toJSON = \case
		POpponentMove x -> toJSON x
		PMyMove x -> toJSON x
		PGameStatus x -> toJSON x
		PCrowd x -> toJSON x
instance FromJSON LichessMsgPayload

data Crowd = Crowd {
	white    :: Bool,
	black    :: Bool,
	watchers :: Maybe String } deriving (Show,Generic)
instance ToJSON   Crowd 
instance FromJSON Crowd

data OpponentMove = OpponentMove {
	uci   :: MoveFromTo,
	san   :: String,
	fen   :: String,
	ply   :: Int,
	dests :: Dests }
	deriving (Generic,Show)
instance ToJSON   OpponentMove
instance FromJSON OpponentMove

opponentMove2Move pos opponentmove = head [ move |
	move@Move{..} <- moveGen pos, moveFrom==from, moveTo==to, movePromote==mb_promote ]
	where
	MoveFromTo from to mb_promote = uci (opponentmove::OpponentMove)

data Dests = Dests [(Coors,[Coors])] deriving (Show,Generic)
instance ToJSON Dests where
	toJSON (Dests m) = object $ map (\ (coors,targetcoorss) ->
		(T.pack $ show coors,String $ T.pack $ concatMap show targetcoorss)) m
instance FromJSON Dests where
	parseJSON = withObject "Dests" $ \ o -> Dests <$> pure
		(map (\ (coorstext,String targetcoorss) -> (read $ T.unpack coorstext,parse_tcoors [] $ T.unpack targetcoorss)) (HM.toList o))
		where
		parse_tcoors acc "" = acc
		parse_tcoors acc s | [(tcoors,r)] <- reads s = parse_tcoors (tcoors:acc) r

data TargetCoors = TargetCoors Coors (Maybe Piece)
instance Show TargetCoors where
	show (TargetCoors coors mb_piece) = show coors ++ case mb_piece of
		Nothing -> ""
		Just Ú -> "n"
		Just Û -> "b"
		Just Ü -> "r"
		Just Ý -> "q"
instance Read TargetCoors where
	readsPrec _ s = [ (TargetCoors coors mb_piece, r) |
		(coors,s1) <- reads s,
		let (mb_piece,r) = case s1 of
			(p:r) | Just piece <- lookup p [('n',Ú),('b',Û),('r',Ü),('q',Ý)] -> (Just piece,r)
			s -> (Nothing,s) ]

data MoveFromTo = MoveFromTo Coors Coors (Maybe Piece) deriving Show
instance ToJSON MoveFromTo where
	toJSON (MoveFromTo from to mb_fig) = String $ T.pack $ show from ++ show (TargetCoors to mb_fig)
instance FromJSON MoveFromTo where
	parseJSON = withText "MoveFromTo" $ \ s -> pure $ head [ MoveFromTo from to mb_piece |
		(from,s1) <- reads $ T.unpack s,
		(TargetCoors to mb_piece,"") <- reads s1 ]

data MyMove = MyMove {
		u :: MoveFromTo }
	deriving (Generic,Show)
instance ToJSON   MyMove
instance FromJSON MyMove

instance (FromJSON a,ToJSON a) => WebSocketsData a where
	fromLazyByteString = either error Prelude.id . eitherDecode
	toLazyByteString   = encode

instance {-# OVERLAPS #-} FromJSON Coors where
	parseJSON = withText "Coors" $ parse_coors . T.unpack where
		parse_coors s | [((file,rank),"")] <- reads s = pure (file,rank)
		parse_coors s = fail $ show s ++ " : expected Coors"
instance {-# OVERLAPS #-} ToJSON Coors where
	toJSON coors = String $ T.pack $ show coors

{-
	parseJSONList = withText "List of Coors" $ parse_coors_list [] . T.unpack where
		parse_coors_list acc s | [((file,rank),r)] <- reads s = parse_coors_list (acc++[(file,rank)]) r
		parse_coors_list _ s = fail $ show s ++ " : expected [Coors]"
-}

{-
data NoMessage = NoMessage deriving Show
instance FromJSON NoMessage where
	parseJSON Null = pure NoMessage
instance ToJSON NoMessage where
	toJSON NoMessage = Null
-}



{-
{"v":9,"t":"move","d":{
	"uci":"h2h3",
	"san":"h3",
	"fen":"r2qkbnr/ppp2ppp/2np4/1B2p3/4P1b1/P4N1P/1PPP1PP1/RNBQK2R",
	"ply":9,
	"dests":{"a8":"b8c8","f8":"e7","e8":"e7d7","f7":"f6f5","d8":"d7c8b8e7f6g5h4","g7":"g6g5","b7":"b6","a7":"a6a5","d6":"d5","h7":"h6h5","g4":"f5e6d7c8h5f3h3","g8":"f6h6e7"
	}}}
-}
