module Main where

import Formula(CNF, Clause, Solver)
import Solver(solve)
import Data.IntMap.Lazy (toList)
import Text.Read(readMaybe)
import Basic(sat0, dpll)
import System.TimeIt(timeIt)

data SolverType =
  Satan
  | Naive
  | DPLL
  deriving (Show, Read)

main :: IO ()
main =
  loop (Satan) where
    loop :: SolverType -> IO()
    loop s = do
      putStrLn ("Welcome! You are using the " ++ show(s) ++ " solver.")
      putStrLn "Press s to change your solver, f to solve from a file, i for interactive, or e to exit."
      c <- getLine
      case c of
        "s" -> solverLoop s
        "e" -> return ()
        "f" -> do
           putStrLn "Please enter a filpath with a SAT formula in DIMACS format."
           str <- getLine
           contents <- readFile str
           case parseDimacs contents of
            Nothing -> putStrLn "Sorry, I didn't understand that. Please try again." >> (loop s)
            Just cnf -> (timeIt $  putStrLn (case (getSolver s) cnf of
                                    Nothing -> "Unsatisfiable"
                                    Just x -> "Satisfiable: A satsifying assignment is " ++ show (toList x))) >> (loop s)
        "i" -> do
            putStrLn "Please enter a CNF in the form of a list of lists of nonzero numbers."
            putStrLn "For instance, the list '[[1, 2], [-1]]' represents the formula (x_1 \\/ x_2) /\\ not(x_1)"
            str <- getLine
            case (readMaybe str) of
              Nothing -> putStrLn "Sorry, I didn't understand that. Make sure you format your formula as a list. Please try again" >> (loop s)
              Just r ->
                timeIt $ (putStrLn (case ((getSolver s) r) of
                            Nothing -> "Unsatisfiable" 
                            Just x -> "Satisfiable: A satisfying assignment is " ++ show (toList x))) >> (loop s)
        _ -> putStrLn "Sorry, I didn't understand that. Please try again." >> (loop s)
    solverLoop :: SolverType -> IO()
    solverLoop s = do
        putStrLn "Please enter a solver type. Enter 'Satan' for 2 watched literals, 'Naive' for a solver that tries all assignments, or 'DPLL' for a sat solver with unit propogation and pure literal elimination."
        str <- getLine
        case (parseSolver str) of
          Just x -> loop x
          Nothing -> do
            putStrLn "Sorry, I didn't understand that. Please press s to change your solver again, or l to return to the main loop."
            str <- getLine
            case str of 
              "s" -> solverLoop s
              "l" -> loop s


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

parseSolver :: String -> Maybe SolverType
parseSolver = readMaybe

getSolver :: SolverType -> Solver
getSolver Satan = solve
getSolver Naive = sat0
getSolver DPLL = dpll