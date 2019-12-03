module Main where

import Formula(CNF, Clause)
import Solver(solve)
import Data.IntMap.Lazy (toList)
import Text.Read(readMaybe)

main :: IO ()
main = do
  putStrLn "Welcome! Enter f to solve from a file, i for interactive, or enter e to exit."
  c <- getLine
  case c of
    "e" -> return ()
    "f" -> do
       putStrLn "Please enter a filpath with a SAT formula in DIMACS format."
       str <- getLine
       contents <- readFile str
       case parseDimacs contents of
        Nothing -> putStrLn "Sorry, I didn't understand that. Please try again." >> main
        Just cnf -> putStrLn (case solve cnf of
                                Nothing -> "Unsatisfiable"
                                Just x -> "Satisfiable: A satsifying assignment is " ++ show (toList x)) >> main
    "i" -> do
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