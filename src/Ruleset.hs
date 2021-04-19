module Ruleset where

import Helper
import Types

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

-- TODO
-- Just captures
captureTargets :: State -> Piece -> [Piece]
captureTargets state ((t, c), p) = filter (\((ot, oc), op) -> isAdjacent p op && opponent c == oc) (stateBoard state)

-- TODO
-- Not captures
moveTargets :: State -> Piece -> [Position]
moveTargets state ((t, c), p) = filter (isEmptyField state) $ filter (isAdjacent p) allPositions
