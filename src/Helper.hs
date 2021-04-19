{-# LANGUAGE TupleSections #-}

module Helper where

import Data.Maybe (isNothing)
import Data.Set (fromList, toList)
import Distribution.Simple.Utils (safeHead)
import Tuple
import Types

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
