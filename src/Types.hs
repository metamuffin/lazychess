module Types where

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
