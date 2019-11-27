module Solver where

import Data.List (find, delete)
import Watching (Watchlist, WatchedLitsMap)
import Control.Monad.State.Lazy(State, get, put, runState)
import Data.IntMap.Lazy (IntMap, insert, empty, adjust)
import qualified Data.IntMap.Lazy as IntMap
import Data.Map (Map, adjust)
import qualified Data.Map.Lazy as Map
import Formula
import Watching

-- Solver state containing number of variables, current assignment
-- and propagation queue (data structures can change).
data SolverState = SS {
  s_n :: Int,
  s_ass :: Assignment,
  s_propQ :: [Lit],
  s_wl :: Watchlist,
  s_wlm :: WatchedLitsMap
}

-- Attempt to assign given literal to True.
-- If it was already False, return False;
-- Otherwise return True and add its negation
-- to the end of the propQ if it was unassigned.
assume :: Lit -> State SolverState Bool
assume x = do
  ss@(SS {s_ass=a, s_propQ=q}) <- get
  case (val x a) of
    F -> return False
    T -> return True
    U -> (do
      put (ss {s_ass=assign x True a, s_propQ = (neg x):q})
      return True)
      
-- When literal l watched by clause c has been set to False, try
-- to maintain the watched literal invariant for that clause or perform unit
-- propagation if that isn't possible. Returns False if we encounter a conflict.
propagateOneClause :: Lit -> Clause -> State SolverState Bool
-- Should never happen; something has gone very wrong here.
propagateOneClause _ [] = return False
-- The clause became empty; conflict.
propagateOneClause _ [_] = return False
propagateOneClause l c = do
  ss@(SS {s_ass=a, s_wlm=wlm, s_wl=wl, s_propQ=q}) <- get
  if satisfiedByWatchedLit c wlm a 
  then return True 
  else
    case (findNewWatchedLit c wlm a) of
      -- do unit propagation
      Nothing -> do
        case (watchedLits c wlm) of
          -- The clause became empty; conflict. This is redundant with 2nd pattern.
          Left _ -> return False
          
          -- assume the other watched literal
          Right (l1, l2) -> do
            assume impliedLit
              where impliedLit = if l1 == l then l2 else l1
      -- fix watched literal invariant
      Just l' -> do 
        put (ss {s_wlm=wlm', s_wl=wl'}) 
        return True
          where
            -- Make c watch l' instead of l
            updateWatched (l1, l2) = if l1 == l then (l', l2) else (l1, l')
            wlm' = Map.adjust updateWatched c wlm
            -- Remove c from the Watchlist of l
            lQ = delete c (clausesWatching l wl)
            -- Add c to the Watchlist of l'
            l'Q = c : (clausesWatching l' wl)
            wl' = insert l' l'Q (insert l lQ wl)


-- When literal l, watched by a list of clauses, has been set to False, 
-- try to maintain the watched literal invariant for all clauses in the
-- list or perform unit propagation if that isn't possible. Returns
-- False if we ever encounter a conflict.
propagateOneLiteral :: Lit -> [Clause] -> State SolverState Bool
propagateOneLiteral l [] = return True
propagateOneLiteral l (c:cs) = do
  b <- propagateOneClause l c
  if b then do
    b' <- propagateOneLiteral l cs
    return b'
  else
    return False

-- Repeatedly attempt unit propagation until the
-- propagation queue is empty, updating the SolverState.
-- If at any point, we encounter a conflict, then
-- unitPropagate returns False. Otherwise it returns True.
unitPropagate :: State SolverState Bool
unitPropagate = do
  ss@(SS {s_propQ=q, s_wl=wl}) <- get
  case q of
    []    -> return True
    l:ls  -> do
      put (ss {s_propQ=ls})
      b <- propagateOneLiteral l (clausesWatching l wl)
      if b then do
        unitPropagate
      else do
        -- Empty the propQ manually on failure; we are about to backtrack.
        put (ss {s_propQ=[]})
        return False

-- Return the next decision literal, or Nothing
-- if all literals have been assigned.
pickLiteral :: State SolverState (Maybe Lit)
pickLiteral = do
  SS {s_ass=a, s_n=n} <- get
  return (find (\l -> val l a == U) [1..n])

-- Stateful DPLL helper that attempts to find a satisfying
-- assignment given the partial assignment represented by
-- its initial state. It returns True if one is found, and 
-- False if we encounter a conflict.
dpllHelper :: State SolverState Bool
dpllHelper = do
  b <- unitPropagate
  if b then do
    mx <- pickLiteral
    case mx of
      Nothing -> return True
      Just x  -> do 
        ss <- get
        assume x
        bt <- dpllHelper
        if bt then
          return True
        else do
          put ss
          assume (neg x)
          bf <- dpllHelper
          return bf
  else
    return False

initialState :: CNF -> SolverState
initialState c = SS {s_n = maximum (map (\x -> if x == [] then 0 else maximum (map var x)) c), s_ass = IntMap.empty,
            s_propQ = [], s_wl = fst m, s_wlm = snd m} where 
                m = initialWatched c
  
solve :: CNF -> Maybe Assignment
solve [] = Just (IntMap.empty)
solve c = if [] `elem` c then Nothing else
  let (b, s) = runState (dpllHelper) (initialState c)  in
        if b then Just (s_ass s) else Nothing 
