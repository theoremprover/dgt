{-# LANGUAGE UnicodeSyntax,RecordWildCards,FlexibleInstances,TupleSections #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Chess200 where

import Prelude.Unicode
import Data.Char
import Data.Array
import Data.Maybe
import Data.List
import Data.NumInstances
import Data.Ord
import Text.Printf
import Control.Parallel.Strategies

data Colour = White | Black
	deriving (Show,Eq,Enum,Bounded,Ord)

nextColour White = Black
nextColour Black = White

data Piece = Ù | Ú | Û | Ü | Ý | Þ
	deriving (Eq,Enum,Bounded,Ord,Show)

type Board  = Array Coors Square
type Square = Maybe (Colour,Piece)

type Coors = (Int,Int)
instance {-# OVERLAPS #-} Show Coors where
	show (file,rank) = ['a'..]!!(file-1) : show rank

data Position = Position {
	pBoard              :: Board,
	pColourToMove       :: Colour,
	pCanCastleQueenSide :: [Colour],
	pCanCastleKingSide  :: [Colour],
	pEnPassant          :: Maybe (Coors,Coors),
	pHalfmoveClock      :: Int,
	pNextMoveNumber     :: Int }

initialPosition = Position {
	pBoard = boardFromString [
		"âïáòäðàñ",
		"îßîßîßîß",
		"ØçØçØçØç",
		"çØçØçØçØ",
		"ØçØçØçØç",
		"çØçØçØçØ",
		"ÙèÙèÙèÙè",
		"ëÚêÝíÛéÜ" ],
	pColourToMove       = White,
	pCanCastleQueenSide = allOfThem,
	pCanCastleKingSide  = allOfThem,
	pEnPassant          = Nothing,
	pHalfmoveClock      = 0,
	pNextMoveNumber     = 1 }

allOfThem :: (Enum a,Bounded a,Ord a) => [a]
allOfThem = [minBound..maxBound]

boardFromString ranks = array ((1,1),(max_f,max_r)) $
	zip [ (f,r) | r <- reverse [1..max_r], f <- [1..max_f] ] 
		(concatMap read_rank ranks)
	where
	read_rank = map read_square
	(max_f,max_r) = (maximum (map length ranks),length ranks)

(north,east) = ((0,1),(1,0))
(south,west) = (-north,-east)

pawnStep White = north
pawnStep Black = south

baseRank White = 1
baseRank Black = 8

show_square darksquare square = case square of 
	Nothing            | darksquare → 'ç'
	Nothing                         → 'Ø'
	Just (White,piece) | darksquare → "èéêëìí" !! (fromEnum piece)
	Just (White,piece)              → "ÙÚÛÜÝÞ" !! (fromEnum piece)
	Just (Black,piece) | darksquare → "îïðñòó" !! (fromEnum piece)
	Just (Black,piece)              → "ßàáâãä" !! (fromEnum piece)

read_square c = lookup c all_squares where
	all_squares = [ (show_square dark (Just (col,piece)), (col,piece)) |
		col <- allOfThem, piece <- allOfThem, dark <- allOfThem ]

instance Show Position where
	show Position{..} = printf "¡¢¢¢¢¢¢¢¢£\n%s¦±²³´µ¶·¸¨\n%s to do move %i\n"
		(unlines [ show_rank r | r <- reverse [min_r..max_r] ]) (show pColourToMove) pNextMoveNumber
		where
		((min_f,min_r),(max_f,max_r)) = bounds pBoard
		show_rank r = (" ©ª«¬»®¯°" !! r) : [ show_square (mod (f+r) 2 == 0) (pBoard!(f,r)) | f <- [min_f..max_f] ] ++ "¥"

addCoors :: Board -> Coors -> (Int,Int) -> Maybe Coors
addCoors board coors offset = case coors+offset of
	coors' | coors' ∈ (indices board) → Just coors'
	_      | otherwise                     → Nothing

data Move =
	Move {
		moveFrom    :: Coors,
		moveTo      :: Coors,
		moveTakes   :: Maybe Coors,
		movePromote :: Maybe Piece } |
	Castling Colour CastlingSide
	deriving Eq
data CastlingSide = Queenside | Kingside deriving Eq

instance Show Move where
	show Move{..} = show moveFrom ++ show moveTo ++ case movePromote of
		Nothing → ""
		Just Ú → "N"
		Just Û → "B"
		Just Ü → "R"
		Just Ý → "Q"
	show (Castling _ Queenside) = "O-O-O"
	show (Castling _ Kingside)  = "O-O"

doMove pos@Position{..} move = pos {

	pBoard              = pBoard // case move of
		Move{..} → ( case moveTakes of
			Nothing → []
			Just take_coors → [(take_coors,Nothing)] ) ++
			[ (moveFrom,Nothing), (moveTo,case movePromote of
				Nothing         → pBoard!moveFrom
				Just promote_to → Just (pColourToMove,promote_to) ) ]
		Castling _ Queenside → [ ((1,base),Nothing), ((4,base),pBoard!(1,base)), ((5,base),Nothing), ((3,base),pBoard!(5,base)) ]
		Castling _ Kingside  → [ ((8,base),Nothing), ((6,base),pBoard!(8,base)), ((5,base),Nothing), ((7,base),pBoard!(5,base)) ],
	pColourToMove       = nextColour pColourToMove,

	pHalfmoveClock      = case move of
		Move{..} | Just _      <- moveTakes       → 0
		Move{..} | Just (_,Ù) <- pBoard!moveFrom → 0
		_        | otherwise                      → pHalfmoveClock + 1,

	pNextMoveNumber     = if pColourToMove==Black then pNextMoveNumber+1 else pNextMoveNumber,

	pEnPassant          = case move of
		Move from to Nothing Nothing |
			Just (_,Ù) <- pBoard!from,
			Just to == addCoors pBoard from (2*pawn_step),
			Just middle <- addCoors pBoard from pawn_step → Just (middle,to)
		_ | otherwise                                     → Nothing,

	pCanCastleQueenSide = (if forfeit_queenside then delete pColourToMove else id) pCanCastleQueenSide,
	pCanCastleKingSide  = (if forfeit_kingside  then delete pColourToMove else id) pCanCastleKingSide  }

	where

	pawn_step = pawnStep pColourToMove
	base = baseRank pColourToMove
	(forfeit_queenside,forfeit_kingside) = case move of
		Castling _  _                      → (True, True )
		Move (1,rank) _ _ _ | rank == base → (True, False)
		Move (5,rank) _ _ _ | rank == base → (True, True )
		Move (8,rank) _ _ _ | rank == base → (False,True )
		_                   | otherwise    → (False,False)

moveGen pos@Position{..} = filter king_not_in_check $ potentialMoves pos where
	king_not_in_check move = all (coorsNotInCheck pos_after_move) $ case move of
		Move{..}             → [ kingsCoors pos_after_move ]
		Castling _ Queenside → map (,baseRank pColourToMove) [3..5]
		Castling _ Kingside  → map (,baseRank pColourToMove) [5..7]
		where
		pos_after_move = (doMove pos move) { pColourToMove = pColourToMove }

coorsNotInCheck pos@Position{..} coors = all (≠coors) [ moveTo |
	Move{..} <- potentialMoves $ pos { -- Castling moves do not take a piece, hence it is not considered here
		pColourToMove = nextColour pColourToMove,
		pBoard = pBoard // [ (coors,Just (pColourToMove,Ý)) ] } ]  -- Place some figure at coors in order to also catch pawn takes

kingsCoors Position{..} = head [ coors | (coors,Just (col,Þ)) <- assocs pBoard, col == pColourToMove ]

potentialMoves Position{..} = normal_moves ++ castling_moves where

	normal_moves = [ Move src dest mb_takes mb_promote |
		(src,Just (colour,piece)) <- assocs pBoard,
		colour==pColourToMove,
		(dest@(_,to_rank),mb_takes) <- let
			initial_rank = if pColourToMove==White then 2 else 7
			pawn_step    = pawnStep pColourToMove
			diagonals    = [ north+east,north+west,south+east,south+west ]
			straights    = [ north,west,south,east ]
			knight_moves = [ north+2*east,north+2*west,2*north+west,2*north+east,south+2*west,south+2*east,2*south+west,2*south+east ]
			maybe_move from δ = case addCoors pBoard from δ of
				Nothing   → []
				Just dest → case pBoard!dest of
					Nothing                            → [ (dest,Nothing) ]
					Just (col,_) | col ≠ pColourToMove → [ (dest,Just dest) ]
					_            | otherwise           → []
			maybe_move_direction from δ = case maybe_move from δ of
				moves@[ (_,Just _)   ] → moves
				moves@[ (to,Nothing) ] → moves ++ maybe_move_direction to δ
				_ | otherwise          → []
			in case piece of
				Ù → pawn_moves ++ pawn_takes where
					Just dest   = addCoors pBoard src pawn_step   -- There cannot be a pawn on the opposite base rank
					pawn_moves  = if square_empty dest then [ (dest,Nothing) ] ++ pawn_double else []
					pawn_double = case addCoors pBoard src (2*pawn_step) of
						Just dest2 | square_empty dest2 ∧ snd src == initial_rank → [ (dest2,Nothing) ]
						_          | otherwise                                    → []
					pawn_takes  = [ (dest,Just take_on) |
						Just dest <- map (addCoors pBoard src) [pawn_step+east,pawn_step+west],
						take_on   <- case pBoard!dest of
							Just (colour,_) | colour ≠ pColourToMove                                 → [ dest ]
							Nothing         | Just (middle,pawn_coors) <- pEnPassant, middle == dest → [ pawn_coors ]
							_               | otherwise                                              → [] ]
				Ú → concatMap (maybe_move           src) knight_moves
				Û → concatMap (maybe_move_direction src) diagonals
				Ü → concatMap (maybe_move_direction src) straights
				Ý → concatMap (maybe_move_direction src) (straights++diagonals)
				Þ → concatMap (maybe_move           src) (straights++diagonals),
		mb_promote <- case piece of
			Ù | to_rank == baseRank (nextColour pColourToMove) → map Just [Ý,Ú,Û,Ü]
			_ | otherwise                                      → [ Nothing ] ]

	castling_moves = [ Castling pColourToMove Kingside  | pColourToMove ∈ pCanCastleKingSide,  all square_empty [(6,base),(7,base)] ] ++
		             [ Castling pColourToMove Queenside | pColourToMove ∈ pCanCastleQueenSide, all square_empty [(4,base),(3,base),(2,base)] ]

	base = baseRank pColourToMove
	square_empty = isNothing . (pBoard!)

data MatchResult = Winner Colour WinReason | Draw DrawReason    deriving Show
data WinReason   = Resignation | Checkmate                      deriving Show
data DrawReason  = Fifty_Halfmoves | Stalemate | NoMatePossible deriving Show

type Rating = Float
mAX         = 10000.0
mIN         = negate mAX
eQUAL       =     0.0

rate :: Position → (Rating,Maybe MatchResult)

rate Position{..} | pHalfmoveClock ≥ 50 = (eQUAL,Just $ Draw Fifty_Halfmoves)

rate pos@Position{..} | moveGen pos == [] = case (king_in_check,pColourToMove) of
	(False,_    ) → (eQUAL,Just $ Draw Stalemate)
	(True ,White) → (mIN,  Just $ Winner Black Checkmate)
	(True ,Black) → (mAX,  Just $ Winner White Checkmate)
	where
	king_in_check = not $ coorsNotInCheck pos $ kingsCoors pos

rate pos@Position{..} | max_one_light_figure = (eQUAL,Just $ Draw NoMatePossible) where
	max_one_light_figure = case sort $ filter ((≠Þ).snd) $ catMaybes $ elems pBoard of
		[]                                                                     → True
		[(_,fig)]                 | all_light_figures [fig]                    → True
		[(col1,fig1),(col2,fig2)] | col1≠col2 ∧ all_light_figures [fig1,fig2]  → True
		_                         | otherwise                                  → False
	all_light_figures = all (∈ [Ú,Û])

rate pos = (rating,Nothing) where
	rating = 0.01*mobility +
		sum [ (if colour==White then id else negate) (piece_val piece colour coors) |
			(coors,Just (colour,piece)) <- assocs $ pBoard pos ]

	piece_val fig colour (_,rank) = case fig of
		Ù → 1 + case abs $ fromEnum rank - fromEnum (baseRank (nextColour colour)) of
			1             → 4
			2             → 2
			_ | otherwise → 0
		Ú → 3
		Û → 3
		Ü → 5
		Ý → 9
		Þ → 0

	mobility = fromIntegral $
		length (potentialMoves $ pos { pColourToMove = White }) -
		length (potentialMoves $ pos { pColourToMove = Black })

type Depth = Int
type Line  = [Move]

search :: Bool → Depth → Position → Line → (Rating,Line)
search _        depth pos line | moveGen pos == [] ∨ depth==0 = (fst $ rate pos,line)
search parallel depth pos line = minimax (comparing fst) $ functor go_deeper $ moveGen pos
	where
	functor        = if parallel then parMap rpar else map
	minimax        = if pColourToMove pos == White then maximumBy else minimumBy
	go_deeper move = search False (depth-1) (doMove pos move) (move:line)

