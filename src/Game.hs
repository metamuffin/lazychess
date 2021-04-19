{-# LANGUAGE TupleSections #-}

module Game where

import Data.Maybe (isNothing)
import Data.Set (fromList, toList)
import Distribution.Simple.Utils (safeHead)
import Helper
import Ruleset
import Tuple
import Types

-- TODO Not gonna have special moves
{- TODO Castling
        Promotion
        En Passent
        Everything
-}

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

applyMove :: (Piece, Position) -> State -> State
applyMove (oldpiece@(piecekind, oldpos), newpos) state =
  state
    { stateBoard = new_piece : rest,
      stateActivePlayer = opponent $ stateActivePlayer state
    }
  where
    rest = filter (/= oldpiece) $ stateBoard state
    new_piece = (piecekind, newpos)

applyCapture :: (Piece, Piece) -> State -> State
applyCapture (oldpiece@(piecekind, oldpos), victimpiece@(_, newpos)) state =
  state
    { stateBoard = new_piece : rest,
      stateActivePlayer = opponent $ stateActivePlayer state
    }
  where
    rest = filter (/= oldpiece) $ filter (/= victimpiece) $ stateBoard state
    new_piece = (piecekind, newpos)

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
