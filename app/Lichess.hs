module Lichess where

import Network.HTTP.Conduit

login username password = do
	response <- simpleHTTP $ postRequestWithBody "https://lichess.org/login"