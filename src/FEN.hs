{-# LANGUAGE RecordWildCards,UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module FEN where

import Text.Parsec hiding (Line)
import Text.Parsec.String
import Data.Char
import Data.Maybe
import Data.List
import Control.Monad
import Data.Array

import Chess200


fENFigureChars = [(Ù,"P"),(Ú,"N"),(Û,"B"),(Ü,"R"),(Ý,"Q"),(Þ,"K")]

toFEN :: Position -> String
toFEN Position{..} =
	intercalate "/" [ row2fen 0 [ pBoard!(f,r) | f <- [1..8] ] | r <- [8,7..1] ] ++ " " ++
	(if pColourToMove==White then "w" else "b") ++ " " ++
	(if castles=="" then "-" else castles) ++ " " ++
	maybe "-" show pEnPassantMiddle ++ " " ++
	show pHalfmoveClock ++ " " ++ show pNextMoveNumber
	where
	castles =
		(if White `elem` pCanCastleKingSide  then "K" else "") ++
		(if White `elem` pCanCastleQueenSide then "Q" else "") ++
		(if Black `elem` pCanCastleKingSide  then "k" else "") ++
		(if Black `elem` pCanCastleQueenSide then "q" else "")
	row2fen :: Int -> [Maybe (Colour,Piece)] -> String
	row2fen 0 [] = ""
	row2fen cnt [] = show cnt
	row2fen cnt (Nothing:rs) = row2fen (cnt+1) rs
	row2fen cnt ((Just (col,piecetype)) : rs) = (if cnt>0 then show cnt else "") ++
		map (if col==White then toUpper else toLower) (fromJust $ lookup piecetype fENFigureChars) ++
		row2fen 0 rs

fromFEN :: String -> Either ParseError Position
fromFEN = parse fen_p ""

fenpos_p :: Parsec String u Position
fenpos_p = do
	rows <- count 8 $ manyTill (digit_p <|> piece_p) (string " " <|> string "/")
	colour <- (string "w" >> return White) <|> (string "b" >> return Black)
	string " "
	castleK <- option False $ string "K" >> return True
	castleQ <- option False $ string "Q" >> return True
	castlek <- option False $ string "k" >> return True
	castleq <- option False $ string "q" >> return True
	when (not $ castleK || castleQ || castlek || castleq) $ string "-" >> return ()
	string " "
	mb_ep <- (string "-" >> return Nothing) <|> (ep_square_p >>= return . Just)
	return $ Position
		(array ((1,1),(8,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [1..8] ] (concat $ concat rows))
		colour
		(if castleQ then [White] else [] ++ if castleq then [Black] else [])
		(if castleK then [White] else [] ++ if castlek then [Black] else [])
		mb_ep 0 0
	where
	digit_p = do
		d <- digit
		return $ replicate (read [d]) Nothing
	piece_p = do
		c <- oneOf "KQRBNPkqrbnp"
		return [ Just (if isUpper c then White else Black,
			head [ p | (p,[pc]) <- fENFigureChars, pc == toUpper c ]) ]
	ep_square_p = do
		fc <- satisfy (`elem` ['a'..'h'])
		rc <- string "3" <|> string "7"
		return (ord fc - ord 'a' + 1,read rc)

fen_p = do
	pos <- fenpos_p
	string " "
	halfmoves <- int_p
	string " "
	movecnt <- int_p
	return $ pos { pHalfmoveClock = halfmoves, pNextMoveNumber = movecnt }

int_p = many1 digit >>= (return . (read :: String -> Int))
