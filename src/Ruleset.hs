module Ruleset where

import Data.List
import Data.Maybe
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

posAdd (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

pawnDirection White = (0, 1)
pawnDirection Black = (0, -1)

-- Just captures
captureTargets :: State -> Piece -> [Piece]
captureTargets state piece@((t, c), p) = filter (\(_, pos) -> fmap snd (atPosition state pos) == Just (opponent c)) $ captureTargetsRaw state piece

-- Not captures
moveTargets :: State -> Piece -> [Position]
moveTargets state piece@(_, pos) = filter (isEmptyField state) $ moveTargetsRaw state piece

moveTargetsRaw :: State -> Piece -> [Position]
captureTargetsRaw :: State -> Piece -> [Piece]
-- asdasd

moveTargetsRaw state ((Pawn, color), pos) = [posAdd pos $ pawnDirection color]

captureTargetsRaw state ((Pawn, color), pos) =
  mapMaybe
    (atPositionKeepPos state)
    [ posAdd pos (posAdd (1, 0) $ pawnDirection color),
      posAdd pos (posAdd (-1, 0) $ pawnDirection color)
    ]



