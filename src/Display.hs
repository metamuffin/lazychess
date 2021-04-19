{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Display where

import Ruleset

instance Show State where
  show state = "Current player: " ++ pChar ++ "\n" ++ showBoard (stateBoard state)
    where
      pChar = if stateActivePlayer state == Black then "b" else "W"

pieceChar :: PieceKind -> String
pieceChar (Pawn, Black) = "p"
pieceChar (Pawn, White) = "P"
pieceChar (Rook, Black) = "r"
pieceChar (Rook, White) = "R"
pieceChar (Bishop, Black) = "b"
pieceChar (Bishop, White) = "B"
pieceChar (King, Black) = "k"
pieceChar (King, White) = "K"
pieceChar (Queen, Black) = "q"
pieceChar (Queen, White) = "Q"
pieceChar (Knight, Black) = "n"
pieceChar (Knight, White) = "N"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

showBoard :: [(PieceKind, Position)] -> String
-- showBoard = foldl (\a v -> a ++ "\n" ++ (foldl (\av c -> c ++ " " ++ av) "" v)) "" $ map (map pieceChar)
showBoard board =
  foldl (\ar r -> ar ++ "\n" ++ foldl (\ac c -> ac ++ " " ++ charAtPos c) "" r) "" $ map (\x -> map (,x) [0 .. 8]) [0 .. 8]
  where
    atPos :: (Int, Int) -> Maybe PieceKind
    atPos p = safeHead $ map fst $ filter (\(_, po) -> po == p) board
    charAtPos :: (Int, Int) -> String
    charAtPos pos = maybe " " pieceChar (atPos pos)

{-
locToList :: Int -> Int -> [(a, (Int, Int))] -> a -> [[a]]
locToList x y [] a = replicate y . replicate x $ a
locToList x y [v, (vx, vy)] a = replicate y . replicate x $ a
fillGrid :: [[a]] -> [(a, (Int, Int))] -> [[a]]
fillGrid g [] = g
fillGrid g ((v, (x,y)):xs) = modifyAt y (modifyAt x (const v)) g
-}

readActionUntilValid :: State -> IO Action
readActionUntilValid state =
  maybe (readActionUntilValid state) return =<< readAction state

readAction :: State -> IO (Maybe Action)
readAction state = fmap (parseAction state) getLine

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

coordsToAction :: State -> Position -> Position -> Maybe Action
coordsToAction state fpos tpos = safeHead $ map fst $ filter (\(action, move) -> move == (fpos, tpos)) $ possibleActionsExtented state

parseAction :: State -> String -> Maybe Action
parseAction state input =
  coordsToAction state (parseAlgebraic from) (parseAlgebraic to)
  where
    (from, to) = first2 $ wordsWhen (== '-') input

first2 :: [a] -> (a, a)
first2 l = (head l, head $ tail l)

parseAlgebraic :: String -> (Int, Int)
parseAlgebraic input =
  (p1 c1, p2 c2)
  where
    c1 = head input
    c2 = head $ tail input
    p1 'a' = 0
    p1 'b' = 1
    p1 'c' = 2
    p1 'd' = 3
    p1 'e' = 4
    p1 'f' = 5
    p1 'g' = 6
    p1 'h' = 7
    p1 _ = error "Invalid character in algebraic input"
    p2 '1' = 0
    p2 '2' = 1
    p2 '3' = 2
    p2 '4' = 3
    p2 '5' = 4
    p2 '6' = 5
    p2 '7' = 6
    p2 '8' = 7
    p2 _ = error "Invalid character in algebraic input"

showPosition :: Position -> String
showPosition (x, y) =
  p1 x ++ p2 y
  where
    p1 0 = "a"
    p1 1 = "b"
    p1 2 = "c"
    p1 3 = "d"
    p1 4 = "e"
    p1 5 = "f"
    p1 6 = "g"
    p1 7 = "h"
    p2 0 = "1"
    p2 1 = "2"
    p2 2 = "3"
    p2 3 = "4"
    p2 4 = "5"
    p2 5 = "6"
    p2 6 = "7"
    p2 7 = "8"

showMove :: (Position, Position) -> String
showMove (from, to) = showPosition from ++ "-" ++ showPosition to
 