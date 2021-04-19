{-# LANGUAGE TupleSections #-}

module Ruleset where

import Data.Maybe (isNothing)
import Data.Set (fromList, toList)
import Distribution.Simple.Utils (safeHead)
import Tuple

data PieceType = Pawn | Rook | King | Queen | Knight | Bishop deriving (Eq)

data Color = Black | White deriving (Eq)

type Position = (Int, Int)

type PieceKind = (PieceType, Color)

type Piece = (PieceKind, Position)

type Board = [Piece]

data State = State
  { stateBoard :: Board,
    stateActivePlayer :: Color
  }

type Action = (State -> State)

-- TODO Not gonna have special moves
{- TODO Castling
        Promotion
        En Passent
        Everything
-}

-- Half of the starting chess game
boardHalf :: Color -> [Piece]
boardHalf color =
  [ ((Rook, color), (0, l)),
    ((Knight, color), (1, l)),
    ((Bishop, color), (2, l)),
    ((King, color), (3, l)),
    ((Queen, color), (4, l)),
    ((Bishop, color), (5, l)),
    ((Knight, color), (6, l)),
    ((Rook, color), (7, l))
  ]
    ++ map (\x -> ((Pawn, color), (x, lp))) [0 .. 7]
  where
    l
      | color == White = 0
      | color == Black = 7
    lp
      | color == White = 1
      | color == Black = 6

-- Start of chess game
initialState :: State
initialState =
  State
    { stateBoard = boardHalf Black ++ boardHalf White,
      stateActivePlayer = White
    }

-- Remove duplicates
deduplicate :: Ord a => [a] -> [a]
deduplicate = toList . fromList

-- Is this the same piece type?
isPieceKind :: Piece -> Piece -> Bool
isPieceKind = onFst (==)

-- Is this the same position?
isPosition :: Piece -> Piece -> Bool
isPosition = onSnd (==)

-- Piece at position
atPosition :: State -> Position -> Maybe PieceKind
atPosition state pos = safeHead $ map fst $ filter (\(_, p) -> p == pos) (stateBoard state)

-- 8-neighbourhood
isAdjacent :: Position -> Position -> Bool
isAdjacent (ax, ay) (bx, by) = abs (ax - bx) <= 1 && abs (ay - by) <= 1

-- Opposite colour
opponent :: Color -> Color
opponent Black = White
opponent White = Black

-- Tile has no piece
isEmptyField :: State -> Position -> Bool
isEmptyField state pos = isNothing $ atPosition state pos

allPositions :: [Position]
allPositions = concatMap (\x -> map (x,) [0 .. 7]) [0 .. 7]

-- TODO
-- Just captures
captureTargets :: State -> Piece -> [Piece]
captureTargets state ((t, c), p) = filter (\((ot, oc), op) -> isAdjacent p op && opponent c == oc) (stateBoard state)

-- TODO
-- Not captures
moveTargets :: State -> Piece -> [Position]
moveTargets state ((t, c), p) = filter (isEmptyField state) $ filter (isAdjacent p) allPositions

-- All kings of a certain player
kingsOf :: State -> Color -> [Position]
kingsOf state color = map snd $ filter (\((t, c), _) -> c == color && t == King) (stateBoard state)

-- All pieces of the active player
activePieces :: State -> [Piece]
activePieces state = filter (\((_, c), _) -> c == stateActivePlayer state) (stateBoard state)

possibleMoves :: State -> [(Piece, Position)]
possibleMoves state = concatMap (\p -> map (p,) $ moveTargets state p) (activePieces state)

possibleCaptures :: State -> [(Piece, Piece)]
possibleCaptures state = concatMap (\p -> map (p,) $ captureTargets state p) (activePieces state)

--
applyMove :: (Piece, Position) -> State -> State
applyMove (piece, end) state = state {stateBoard = piece : rest, stateActivePlayer = opponent $ stateActivePlayer state}
  where
    rest = filter (/= piece) $ stateBoard state

-- (,) ((,) a b) c === ((a,b),c) =? (a,b,c) === (,,) a b c

applyCapture :: (Piece, Piece) -> State -> State
applyCapture (murderer, victim) state = state {stateBoard = murderer : rest, stateActivePlayer = opponent $ stateActivePlayer state}
  where
    rest = filter (not . isPosition victim) $ filter (/= murderer) $ stateBoard state

possibleActions :: State -> [State -> State]
possibleActions = map fst . possibleActionsExtented

-- Lists all possible actions and the corresponding 'move' a player needs to input in order to do this action
possibleActionsExtented :: State -> [(Action, (Position, Position))]
possibleActionsExtented state =
  moves ++ captures
  where
    moves = map (\(piece@(_, spos), tpos) -> (applyMove (piece, tpos), (spos, tpos))) (possibleMoves state)
    captures = map (\(mpiece@(_, spos), vpiece@(_, tpos)) -> (applyCapture (mpiece, vpiece), (spos, tpos))) (possibleCaptures state)

-- Is any king in check
isCheck :: State -> Bool
isCheck state = any (\(_, (_, victimpos)) -> victimpos `elem` kingsOf state (stateActivePlayer state)) $ possibleCaptures state

-- Does every possible move result in a check from this state?
isCheckmate :: State -> Bool
isCheckmate state = all (isCheck . ($ state)) (possibleActions state)

-- Would I be put in check after making such a move?
isLegalAction :: State -> (State -> State) -> Bool
isLegalAction state action =
  not $ isCheck state_after
  where
    state_after = action state

--
withActionApplied :: State -> (State -> State) -> Maybe State
withActionApplied state action
  | isLegalAction state action = Just $ action state
  | otherwise = Nothing
