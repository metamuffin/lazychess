module Lib where

import Control.Monad
import Display
import Ruleset
import Tuple

someFunc :: IO ()
someFunc = void $ gameLoop initialState

gameLoop :: State -> IO State
gameLoop state = do
  putStrLn "Possible moves are:"
  putStrLn $ foldr (\a v -> a ++ "\n" ++ v) "" $ map showMove $ map snd $ possibleActionsExtented state
  print state
  action <- readActionUntilValid state
  let new_state = action state
  gameLoop new_state
