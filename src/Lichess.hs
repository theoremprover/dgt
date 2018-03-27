{-# LANGUAGE OverloadedStrings,RecordWildCards,ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lichess where

import Network.HTTP.Simple
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (Status(..))
import qualified Data.ByteString.Char8 as BS
import Data.Aeson
import Data.Aeson.Types (explicitParseField)
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Monad
import Control.Monad.IO.Class (MonadIO,liftIO)
import Control.Monad.Trans.State
import Data.CaseInsensitive (mk)

import Data.Time.Clock.POSIX
import Data.Time.Clock

import Chess200

data LichessState = LichessState {
	lisAuthCookie :: String,
	myColour      :: Colour } deriving Show

type LichessM m a = StateT LichessState m a

rawLichessRequest :: (FromJSON val,MonadIO m) => String -> [(String,Maybe String)] -> [(String,String)] -> m (Response BS.ByteString,val)
rawLichessRequest path querystring headers = do
	response <- liftIO $ httpBS $
		setRequestMethod "POST" $
		setRequestPath (BS.pack path) $
		setRequestQueryString (map (\(a,b) -> (BS.pack a,fmap BS.pack b)) querystring) $
		setRequestSecure True $
		setRequestPort 443 $
		setRequestHeaders ([("Accept","application/vnd.lichess.v3+json")] ++ map (\(a,b) -> (mk (BS.pack a),BS.pack b)) headers) $
		setRequestHost "lichess.org" $
		defaultRequest	
	let bs = getResponseBody response
	case eitherDecodeStrict bs of
		Left errmsg -> do
			liftIO $ putStrLn $ "rawLichessRequest response body: " ++ BS.unpack bs
			error $ "rawLichessRequest eitherDecodeStrict: " ++ errmsg
		Right val -> return (response,val)

lichessRequestL :: (FromJSON val,MonadIO m) => String -> [(String,Maybe String)] -> LichessM m (Status,val)
lichessRequestL path querystring = do
	authcookie <- gets lisAuthCookie
	(response,val) <- rawLichessRequest path querystring [("Cookie",authcookie)]
	liftIO $ BS.putStrLn $ getResponseBody response
	return (getResponseStatus response,val)

withLoginL :: (MonadIO m) => String -> String -> LichessM m a -> m a
withLoginL username password lichessm = do
	(response,user::User) <- rawLichessRequest "/login" [("username",Just username),("password",Just password)] []
	let Status{..} = getResponseStatus response
	case statusCode == 200 of
		False -> error $ "withLoginL: " ++ BS.unpack statusMessage
		True  -> do
			liftIO $ putStrLn "OK, logged in."
			
			let [Cookie{..}] = destroyCookieJar $ responseCookieJar response
			evalStateT lichessm $ LichessState {
				lisAuthCookie = BS.unpack cookie_name ++ "=" ++ BS.unpack cookie_value,
				myColour = White }

startGameL :: (MonadIO m) => Maybe Position -> Maybe Colour -> LichessM m (Maybe Game)
startGameL mb_position mb_colour = do
	(Status{..},game) <- lichessRequestL "/setup/ai" [
		("color",Just "white" ), -- $ maybe "random" show mb_colour),
		("days",Just "2"),("time",Just "5.0"),
		("fen",Just "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"),
		("increment",Just "8"),
		("level",Just "2"),
		("timeMode",Just "0"),
		("variant",Just "1") ]
	liftIO $ putStrLn $ "StatusCode: " ++ show statusCode
	return game

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
}
-}

data Perf = Perf {
	perfGames  :: Int,
	perfRating :: Int,
	perfRd     :: Int,
	perfProg   :: Int,
	perfProv   :: Bool } deriving Show
instance FromJSON Perf where
	parseJSON (Object v) = Perf <$>
		v .:  "games" <*>
		v .:  "rating" <*>
		v .:  "rd" <*>
		v .:  "prog" <*>
		v .:? "prov" .!= False

data Perfs = Perfs [(String,Perf)] deriving Show
instance FromJSON Perfs where
	parseJSON (Object v) = Perfs <$> do
		forM (HM.toList v) $ \ (t,p) -> do
			pp <- parseJSON p
			return (T.unpack t,pp)

data Profile = Profile {
	profileCountry   :: String,
	profilefirstName :: String } deriving Show
instance FromJSON Profile where
	parseJSON (Object v) = Profile <$>
		v .: "country" <*>
		v .: "firstName"

data PlayTime = PlayTime {
	playtimeTotal :: POSIXTime,
	playtimeTV    :: POSIXTime } deriving Show
instance FromJSON PlayTime where
	parseJSON (Object v) = PlayTime <$>
		v .: "total" <*>
		v .: "tv"

-- TODO: fold ueber parseJSON?

data User = User {
	userID         :: String,
	userName       :: String,
	userOnline     :: Bool,
	userPerfs      :: Perfs,
	userCreatedAt  :: UTCTime,
	userProfile    :: Profile,
	userSeenAt     :: UTCTime,
	userPlayTime   :: PlayTime,
	userLang       :: String,
	userNowPlaying :: Maybe [Game] } deriving Show
instance FromJSON User where
	parseJSON (Object v) = User <$>
		v .:  "id" <*>
		v .:  "username" <*>
		v .:  "online" <*>
		v .:  "perfs" <*>
		explParse parsePosixSecs v "createdAt" <*>
		v .:  "profile" <*>
		explParse parsePosixSecs v "seenAt" <*>
		v .:  "playTime" <*>
		v .:  "language" <*>
		v .:? "nowPlaying"

parsePosixSecs (Number n) = posixSecondsToUTCTime (realToFrac n / 1000)
explParse f = explicitParseField (pure . f)

data Variant = Variant {
	variantKey :: String,
	variantName :: String } deriving Show
instance FromJSON Variant where
	parseJSON (Object v) = Variant <$>
		v .: "key" <*>
		v .: "name"

data Player = Player {
	playerId       :: Maybe String,
	playerUserName :: String,
	playerAI       :: Maybe Int } deriving Show
instance FromJSON Player where
	parseJSON (Object v) = Player <$>
		v .:  "id" <*>
		v .:  "username" <*>
		v .:? "ai"

data Game = Game {
	gameFullId      :: String,
	gameGameId      :: String,
	gameFEN         :: String,
	gameColor       :: String,
	gameLastMove    :: String,
	gameVariant     :: Variant,
	gameSpeed       :: String,
	gamePerf        :: String,
	gameRated       :: Bool,
	gameOpponent    :: Player,
	gameIsMyTurn    :: Bool,
	gameSecondsLeft :: Maybe Int } deriving Show
instance FromJSON Game where
	parseJSON (Object v) = Game <$>
		v .: "fullId" <*>
		v .: "gameId" <*>
		v .: "fen" <*>
		v .: "color" <*>
		v .: "lastMove" <*>
		v .: "variant" <*>
		v .: "speed" <*>
		v .: "perf" <*>
		v .: "rated" <*>
		v .: "opponent" <*>
		v .: "isMyTurn" <*>
		v .: "secondsLeft"

