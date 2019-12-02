module Main where

import Formula(CNF, Clause)
import Solver(solve)
import Data.IntMap.Lazy (toList)
import Text.Read(readMaybe)

main :: IO ()
main = do
  putStrLn "Welcome! Enter a filename or enter e to exit."
  str <- getLine
  case str of
    "e" -> return ()
    _ -> do
     contents <- readFile str
     case parseDimacs contents of
      Nothing -> putStrLn "Sorry, I didn't understand that. Please try again." >> main
      Just cnf -> putStrLn (case solve cnf of
                              Nothing -> "Unsatisfiable"
                              Just x -> "Satisfiable: A satsifying assignment is " ++ show (toList x)) >> main

parseDimacs :: String -> Maybe CNF
parseDimacs s = foldr (\x acc -> case ((dimacsLine x), acc) of
                                  (Nothing, _) -> acc
                                  (Just y, Just z) -> Just (y : z)
                                  (Just y, _) -> Just [y]) Nothing (lines s)

dimacsLine :: String -> Maybe Clause
dimacsLine ('c' : _) = Nothing
dimacsLine ('p' : _) = Nothing
dimacsLine l = foldr (\x acc -> case (x, acc) of
                                  ("0", Nothing) -> acc
                                  (c, Nothing) -> case readMaybe c of
                                                    Nothing -> Nothing
                                                    Just z -> Just [z]
                                  (c, Just y) ->  case readMaybe c of
                                                    Nothing -> Nothing
                                                    Just z -> Just (z : y)) Nothing (words l) 