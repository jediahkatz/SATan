module Main where

import Solver(solve)
import Data.IntMap.Lazy (toList)
import Text.Read(readMaybe)

main :: IO ()
main = do
  putStrLn "Welcome! Press s to solve a formula or e to exit."
  str <- getLine
  case str of
    "e" -> return ()
    "s" -> do
        putStrLn "Please enter a CNF in the form of a list of lists of nonzero numbers."
        putStrLn "For instance, the list '[[1, 2], [-1]]' represents the formula (x_1 \\/ x_2) /\\ not(x_1)"
        s <- getLine
        case (readMaybe s) of
          Nothing -> putStrLn "Sorry, I didn't understand that. Make sure you format your formula as a list. Please try again" >> main
          Just r ->
            putStrLn (case (solve r) of
                        Nothing -> "Unsatisfiable" 
                        Just x -> "Satisfiable: A satisfying assignment is " ++ show (toList x)) >> main
    _ -> putStrLn "Sorry, I didn't understand that. Please try again." >> main