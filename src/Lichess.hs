{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Lichess where

import Network.HTTP.Simple
import Network.HTTP.Conduit
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

login username password path = do
	response <- httpBS $
		setRequestMethod "POST" $
		setRequestPath "/login" $
		setRequestQueryString [("username",Just (BS.pack username)),("password",Just (BS.pack password))] $
		setRequestSecure True $
		setRequestPort 443 $
		setRequestHeaders [("Accept","application/vnd.lichess.v3+json")] $
		setRequestHost "lichess.org" $
		defaultRequest

	putStrLn $ "The status code was: " ++ show (getResponseStatus response)

	let
		cookiejar = responseCookieJar response
		[cookie] = destroyCookieJar cookiejar
		cookiename = cookie_name cookie
		cookievalue = cookie_value cookie
		cookietxt = cookiename `BS.append` "=" `BS.append` cookievalue

	let bs = getResponseBody response
	--putStrLn (BS.unpack bs)
	case eitherDecodeStrict bs :: Either String User of
		Left errmsg -> putStrLn errmsg
		Right user -> do
			print user

			putStrLn "---------------------"

			response2 <- httpBS $
				setRequestMethod "POST" $
				setRequestPath "/setup/ai" $
				setRequestQueryString [("color",Just "white"),("days",Just "2"),("time",Just "5.0"),("fen",Just "8/8/6k1/B3p1p1/3bP1K1/5PP1/8/8+b+-+-"),("increment",Just "8"),("level",Just "2"),("timeMode",Just "0"),("variant",Just "1")] $
				setRequestSecure True $
				setRequestPort 443 $
				setRequestHeaders [("Accept","application/vnd.lichess.v3+json"),("Cookie",cookietxt)] $
				setRequestHost "lichess.org" $
				defaultRequest --{ cookieJar = Just cookiejar }

			putStrLn $ "The status code was: " ++ show (getResponseStatus response2)

			let bs2 = getResponseBody response2
			putStrLn (BS.unpack bs2)
{-
	case eitherDecodeStrict bs2 :: Either String User of
		Left errmsg -> putStrLn errmsg
		Right x -> print x
-}

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

