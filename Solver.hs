module Solver where

import Data.List (find, delete)
import Watching (Watchlist, WatchedLitsMap)
import Control.Monad.State.Lazy(State, get, put)
import Data.IntMap.Lazy (IntMap, insert)
import Data.Map (Map, adjust)
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
          
          -- add negation of the other watched literal to the propagation queue
          Right (l1, l2) -> do
            put (ss {s_propQ=(impliedLit:q)})
            return True
              where impliedLit = if l1 == l then neg l2 else neg l1
      -- fix watched literal invariant
      Just l' -> do 
        put (ss {s_wlm=wlm', s_wl=wl'}) 
        return True
          where
            -- Make c watch l' instead of l
            updateWatched (l1, l2) = if l1 == l then (l', l2) else (l1, l')
            wlm' = adjust updateWatched c wlm
            -- Remove c from the Watchlist of l
            lQ = delete c (clausesWatching l wl)
            -- Add c to the Watchlist of l'
            l'Q = c : (clausesWatching l' wl)
            wl' = insert l' l'Q (insert l lQ wl)
  

-- Repeatedly attempt unit propagation until the
-- propagation queue is empty, updating the SolverState.
-- If at any point, we encounter a conflict, then
-- unitPropagate returns False. Otherwise it returns True.
unitPropagate :: State SolverState Bool
unitPropagate = undefined
-- do
  -- SS {s_propQ=q, s_wl=wl} <- get
  -- case q of
    -- []     -> return True
    -- -- l has been set to False
    -- (l:ls) ->
      -- map (propagateOneClause l) (clausesWatching l wl)

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
dpllHelper = undefined
--dpllHelper = do
--  b <- unitPropagate
--  // return False if b is False
--  x <- pickLiteral
--  // return True if x is Nothing
--  ss <- get
--  assume x
--  b' <- dpllHelper
--  // return True if b is True
--  put ss
--  assume (not x)
--  b'' <- dpllHelper
--  return b''
  
solve :: CNF -> Maybe Assignment
solve = undefined