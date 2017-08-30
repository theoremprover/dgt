{-# LANGUAGE UnicodeSyntax,RecordWildCards,TypeSynonymInstances,FlexibleInstances,
	TupleSections,StandaloneDeriving #-}

module ChessLib where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char
import Data.Array
import Data.Maybe
import Data.List
import Data.Stack
import Data.NumInstances
import Data.Ord
import Text.Printf
import Control.Parallel.Strategies
import System.IO

data Colour = White | Black
	deriving (Show,Eq,Enum,Bounded,Ord)

nextColour White = Black
nextColour Black = White

data Piece = Ù | Ú | Û | Ü | Ý | Þ
	deriving (Eq,Enum,Bounded,Ord,Show)

type Board = Array Coors Square

type Square = Maybe (Colour,Piece)

data File = A | B | C | D | E | F | G | H
	deriving (Show,Eq,Ix,Ord,Enum)

type Coors = (File,Int)
instance {-# OVERLAPPING #-} Show Coors where
	show (file,rank) = map toLower (show file) ++ show rank

data Position = Position {
	pBoard              :: Board,
	pColourToMove       :: Colour,
	pCanCastleQueenSide :: Set Colour,
	pCanCastleKingSide  :: Set Colour,
	pEnPassant          :: Maybe (Coors,Coors),
	pHalfmoveClock      :: Int,
	pNextMoveNumber     :: Int }

initialPosition = Position {
	pBoard = boardFromString $
		"âïáòäðàñ" ++
		"îßîßîßîß" ++
		"ØçØçØçØç" ++
		"çØçØçØçØ" ++
		"ØçØçØçØç" ++
		"çØçØçØçØ" ++
		"ÙèÙèÙèÙè" ++
		"ëÚêÝíÛéÜ",
	pColourToMove       = White,
	pCanCastleQueenSide = Set.fromList allOfThem,
	pCanCastleKingSide  = Set.fromList allOfThem,
	pEnPassant          = Nothing,
	pHalfmoveClock      = 0,
	pNextMoveNumber     = 1 }

allOfThem :: (Enum a,Bounded a,Ord a) => [a]
allOfThem = enumFromTo minBound maxBound

baseRank White = 1
baseRank Black = 8

(north,east) = ((0,1),(1,0))
(south,west) = (-north,-east)

pawnDir White = north
pawnDir Black = south

show_square darksquare square = case square of 
	Nothing            | darksquare -> 'ç'
	Nothing                         -> 'Ø'
	Just (White,piece) | darksquare -> "èéêëìí" !! (fromEnum piece)
	Just (White,piece)              -> "ÙÚÛÜÝÞ" !! (fromEnum piece)
	Just (Black,piece) | darksquare -> "îïðñòó" !! (fromEnum piece)
	Just (Black,piece)              -> "ßàáâãä" !! (fromEnum piece)

read_square :: Char -> Square
read_square c = lookup c [ (show_square dark (Just (col,piece)), (col,piece)) |
	col <- allOfThem, piece <- allOfThem, dark <- allOfThem ]

boardFromString s = array ((A,1),(H,8)) $ zip [ (f,r) | r <- [8,7..1], f <- [A .. H] ] (map read_square s)

instance Show Position where
	show Position{..} = printf "¿ÀÀÀÀÀÀÀÀÁ\n%sÄÏÐÑÒÓÔÕÖÆ\n%s to do move %i\n"
		(unlines $ map show_rank [8,7..1]) (show pColourToMove) pNextMoveNumber
		where
		show_rank rank = [ "ÇÈÉÊËÌÍÎ" !! (fromEnum rank - 1) ] ++
			[ show_square (is_darksquare (file,rank)) (pBoard!(file,rank)) | file <- [A .. H] ] ++ "Ã"
			where
			is_darksquare (file,rank) = mod (fromEnum rank + fromEnum file) 2 == 1

infixl 6 +++

(+++) :: Coors -> (Int,Int) -> Maybe Coors
(file,rank) +++ (δfile,δrank) |
	ifile' `elem` [0..7] && rank' `elem` [1..8] = Just (toEnum ifile',rank')
	where
	(ifile',rank') = (fromEnum file + δfile, rank + δrank)
_  +++ _ = Nothing

data Move =
	Move {
		moveFrom    :: Coors,
		moveTo      :: Coors,
		moveTakes   :: Maybe Coors,
		movePromote :: Maybe Piece } |
	Castling CastlingSide
data CastlingSide = Queenside | Kingside

instance Show Move where
	show Move{..} = show moveFrom ++ show moveTo ++ case movePromote of
		Nothing -> ""
		Just Ú -> "N"
		Just Û -> "B"
		Just Ü -> "R"
		Just Ý -> "Q"
	show (Castling Queenside) = "O-O-O"
	show (Castling Kingside)  = "O-O"

doMove pos@Position{..} move = pos' {
	pCanCastleQueenSide = if disabled_queenside then Set.delete pColourToMove pCanCastleQueenSide else pCanCastleQueenSide,
	pCanCastleKingSide  = if disabled_kingside  then Set.delete pColourToMove pCanCastleKingSide  else pCanCastleKingSide,
	pColourToMove       = nextColour pColourToMove,

	pHalfmoveClock = case move of
		Move _    _ (Just _) _                              -> 0
		Move from _ _        _ | Just (_,Ù) <- pBoard!from -> 0
		_                                                   -> pHalfmoveClock + 1,

	pNextMoveNumber = pNextMoveNumber + 1,

	pEnPassant = case move of
		Move from to Nothing Nothing |
			Just (_,Ù) <- pBoard!from,
			Just to == from +++ 2*pawn_dir,
			Just middle <- from +++ pawn_dir -> Just (middle,to)
		_                                    -> Nothing }
	where
	pawn_dir = pawnDir pColourToMove 
	(disabled_queenside,disabled_kingside) = case (pColourToMove,move) of
		(_,Castling _)           -> (True, True )
		(White,Move (E,1) _ _ _) -> (True, True )
		(Black,Move (E,8) _ _ _) -> (True, True )
		(White,Move (A,1) _ _ _) -> (True, False)
		(Black,Move (A,8) _ _ _) -> (True ,False)
		(White,Move (H,1) _ _ _) -> (False,True )
		(Black,Move (H,8) _ _ _) -> (False,True )
		_					     -> (False,False)
	pos' = pos { pBoard = pBoard // case move of
		Move{..} -> maybe [] (\ take_coors -> [(take_coors,Nothing)]) moveTakes ++
			[ (moveFrom,Nothing), (moveTo,maybe (pBoard!moveFrom) (Just.(pColourToMove,)) movePromote) ]
		Castling Queenside -> [ ((A,r),Nothing), ((D,r),pBoard!(A,r)), ((E,r),Nothing), ((C,r),pBoard!(E,r)) ]
		Castling Kingside  -> [ ((H,r),Nothing), ((F,r),pBoard!(H,r)), ((E,r),Nothing), ((G,r),pBoard!(E,r)) ] } where
		r = baseRank pColourToMove

data MatchResult = Winner Colour WinReason | Draw DrawReason deriving Show
data WinReason  = Resignation | Checkmate deriving Show
data DrawReason = Fifty_Halfmoves | Stalemate | NoWinPossible deriving Show

moveGen pos@Position{..} = filter king_not_in_check $ potentialMoves pos
	where
	king_not_in_check move = all (coorsNotInCheck pos_after_move pColourToMove) $ case move of
		Castling Queenside -> [(E,r),(D,r),(C,r)]
		Castling Kingside  -> [(E,r),(F,r),(G,r)]
		Move{..}           -> [ kingsCoors pos_after_move pColourToMove ]
		where
		r = baseRank pColourToMove
		pos_after_move = doMove pos move

coorsNotInCheck pos colour coors = all (/=coors) [ moveTo |
	Move{..} <- potentialMoves $ pos {
		-- Say, the next colour would be to move now...
		pColourToMove = nextColour colour,
		-- ... and place some figure at the coors to also get pawn takes as potential moves:
		pBoard = pBoard pos // [ (coors,Just (colour,Ý)) ] } ]

notInCheck pos@Position{..} = coorsNotInCheck pos pColourToMove $ kingsCoors pos pColourToMove

kingsCoors pos colour = head [ coors | (coors,Just (col,Þ)) <- assocs (pBoard pos), col == colour ]

potentialMoves pos@Position{..} = [ Move move_from move_to mb_takes mb_promote |
	(move_from@(_,from_rank),Just (colour,piece)) <- assocs pBoard, colour==pColourToMove,
	(move_to@(_,to_rank),mb_takes) <- let
		initial_rank = if pColourToMove==White then 2 else 7
		pawn_dir = pawnDir pColourToMove 
		diagonals = [ north+east,north+west,south+east,south+west ]
		straights = [ north,west,south,east ]
	 	knight_moves = [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]

		maybe_move from δ = case from +++ δ of
			Nothing -> []
			Just move_to -> case pBoard!move_to of
				Nothing                             -> [ (move_to,Nothing) ]
				Just (col,_) | col /= pColourToMove -> [ (move_to,Just move_to) ]
				_            | otherwise            -> []
		maybe_move_direction from δ = case maybe_move from δ of
			move@[ (_,Just _)   ] -> move
			move@[ (to,Nothing) ] -> move ++ maybe_move_direction to δ
			_ | otherwise         -> []
		in case piece of
			Ù -> ( case move_from +++ pawn_dir of
				Just move_to | Nothing <- pBoard!move_to -> [ (move_to,Nothing) ] ++
					case move_from +++ 2*pawn_dir of
						Just move_to2 | Nothing <- pBoard!move_to2, from_rank==initial_rank -> [ (move_to2,Nothing) ]
						_ -> []
				_            | otherwise -> [] ) ++
				[ (move_to,Just take_on) | Just move_to <- map (move_from +++) [pawn_dir+east,pawn_dir+west],
					take_on <- case pBoard!move_to of
						Just (colour,_) | colour /= pColourToMove                                   -> [ move_to ]
						_               | Just (middle,pawn_coors) <- pEnPassant, middle == move_to -> [ pawn_coors ]
						_               | otherwise                                                 -> [] ]
			Ú -> concatMap (maybe_move           move_from) knight_moves
			Û -> concatMap (maybe_move_direction move_from) diagonals
			Ü -> concatMap (maybe_move_direction move_from) straights
			Ý -> concatMap (maybe_move_direction move_from) (straights++diagonals)
			Þ -> concatMap (maybe_move           move_from) (straights++diagonals),

	mb_promote <- case piece of
		Ù | to_rank == baseRank (nextColour pColourToMove) -> map Just [Ý,Ú,Û,Ü]
		_ | otherwise                                      -> [ Nothing ] ] ++

	let
		r = baseRank pColourToMove
		square_empty coors = isNothing $ pBoard!coors
	in
	[ Castling Kingside  | pColourToMove `Set.member` pCanCastleKingSide,  all square_empty [(F,r),(G,r)] ] ++
	[ Castling Queenside | pColourToMove `Set.member` pCanCastleQueenSide, all square_empty [(D,r),(C,r),(B,r)] ]

type Rating = Float
mAX         = 10000.0
mIN         = negate mAX
eQUAL       =     0.0

rate :: Position -> (Rating,Maybe MatchResult)

rate Position{..} | pHalfmoveClock >= 50 = (eQUAL,Just $ Draw Fifty_Halfmoves)

rate pos | [] <- moveGen pos = case (notInCheck pos,pColourToMove pos) of
	(True,_)      -> (eQUAL,Just $ Draw Stalemate)
	(False,White) -> (mIN,  Just $ Winner Black Checkmate)
	(False,Black) -> (mAX,  Just $ Winner White Checkmate)

rate pos@Position{..} | max_one_light_figure = (eQUAL,Just $ Draw NoWinPossible) where
	all_light_figures = all (`elem` [Ú,Û])
	max_one_light_figure = case sort $ filter ((/=Þ).snd) $ catMaybes $ elems pBoard of
		[]                                                          -> True
		[(_,fig)]                   | all_light_figures [fig]       -> True
		[(White,fig1),(Black,fig2)] | all_light_figures [fig1,fig2] -> True
		_                           | otherwise                     -> False

rate pos = (rating,Nothing) where
	rating = 0.01*mobility + sum [ (if colour==White then id else negate) (piece_val piece colour coors) |
		(coors,Just (colour,piece)) <- assocs $ pBoard pos ]
	piece_val Ù colour (_,rank) = 1 + case abs (rank - baseRank (nextColour colour)) of
				1             -> 4
				2             -> 2
				_ | otherwise -> 0
	piece_val Ú _ _ = 3
	piece_val Û _ _ = 3
	piece_val Ü _ _ = 5
	piece_val Ý _ _ = 9
	piece_val Þ _ _ = 0
	mobility = fromIntegral $
		length (potentialMoves $ pos { pColourToMove = White }) -
		length (potentialMoves $ pos { pColourToMove = Black })

type Depth = Int

search :: Bool -> Depth -> Position -> [Move] -> (Rating,[Move])
search _ maxdepth pos                     line | null (moveGen pos) || maxdepth==0 = (fst $ rate pos,line)
search parallel maxdepth pos@Position{..} line = minimax (comparing fst) (functor deeper $ moveGen pos)
	where
	functor     = if parallel then parMap rpar else map
	minimax     = if pColourToMove == White then maximumBy else minimumBy
	deeper move = search False (maxdepth-1) (doMove pos move) (move:line)
