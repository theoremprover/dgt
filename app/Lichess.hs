{-# LANGUAGE OverloadedStrings #-}
module Lichess where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import Data.Aeson
import Data.Aeson.Types (explicitParseField)
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Control.Monad

import Data.Time.Clock.POSIX
import Data.Time.Clock

login username password = do
	response <- httpBS $ --httpJSON $
		setRequestMethod "POST" $
		setRequestPath "/login" $
		setRequestQueryString [("username",Just (BS.pack username)),("password",Just (BS.pack password))] $
		setRequestSecure True $
		setRequestPort 443 $
		setRequestHeaders [("Accept","application/vnd.lichess.v3+json")] $
		setRequestHost "lichess.org" $
		defaultRequest

	putStrLn $ "The status code was: " ++ show (getResponseStatus response)
	let bs = getResponseBody response
	putStrLn (BS.unpack bs)
	case eitherDecodeStrict bs :: Either String User of
		Left errmsg -> putStrLn errmsg
		Right x -> print x

--	print $ responseCookieJar response
	-- set
{--
	BS.putStrLn (getResponseBody response)
	print response
--}
--	print $ (getResponseBody response :: Value)

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
	userNowPlaying :: [Game] } deriving Show
instance FromJSON User where
	parseJSON (Object v) = User <$>
		v .: "id" <*>
		v .: "username" <*>
		v .: "online" <*>
		v .: "perfs" <*>
		explParse parsePosixSecs v "createdAt" <*>
		v .: "profile" <*>
		explParse parsePosixSecs v "seenAt" <*>
		v .: "playTime" <*>
		v .: "language" <*>
		v .: "nowPlaying"

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

