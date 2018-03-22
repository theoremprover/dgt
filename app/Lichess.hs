{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module Lichess where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import Data.Aeson (Value)

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

	BS.putStrLn (getResponseBody response)
{--
	print response

	print $ (getResponseBody response :: Value)
--}

data Perf = Perf {
	perfGames :: Int,
	perfRating :: Int,
	perfRd :: Int,
	perfProg :: Int,
	perfProv :: Bool }

data User = User {
	userID :: String,
	userName :: String,
	userOnline :: Bool,
	userPerfs :: [(String,Perf)],
	userCreatedAt :: 
{"id":"threetee","username":"Threetee","online":false,
"perfs":
{"blitz":{"games":2004,"rating":1488,"rd":60,"prog":46},
"bullet":{"games":746,"rating":1376,"rd":60,"prog":-2},
"correspondence":{"games":16,"rating":1608,"rd":133,"prog":127,"prov":true},
"puzzle":{"games":2424,"rating":1593,"rd":62,"prog":75},
"classical":{"games":5,"rating":1654,"rd":150,"prog":0,"prov":true},
"rapid":{"games":255,"rating":1654,"rd":63,"prog":0} },

"createdAt":1449173808339,"profile":{"country":"DE","firstName":"Robert"},"seenAt":1521744583804,"playTime":{"total":1485635,"tv":0},"language":"de-DE","nowPlaying":[]}
