import Data.List (find)
import Watching (Watchlist, WatchedLitsMap)
import Control.Monad.State.Lazy(State, get, put)
import Data.IntMap.Lazy (IntMap, insert)
import Formula
import Watching

-- Solver state containing number of variables, current assignment
-- and propagation queue (data structures can change).
data SolverState = SS {
  n :: Int,
  ass :: Assignment,
  propQ :: [Lit],
  wl :: Watchlist,
  wlm :: WatchedLitsMap
}

-- Attempt to assign given literal to True.
-- If it was already False, return False;
-- Otherwise return True and add its negation
-- to the end of the propQ if it was unassigned.
assume :: Lit -> State SolverState Bool
assume x = do
  ss@(SS {ass=a, propQ=q}) <- get
  case (val x a) of
    F -> return False
    T -> return True
    U -> (do
      put (ss {ass=insert x T a, propQ = (neg x):q})
      return True)

-- Repeatedly attempt unit propagation until the
-- propagation queue is empty, updating the SolverState.
-- If at any point, we encounter a conflict, then
-- unitPropagate returns False. Otherwise it returns True.
unitPropagate :: State SolverState Bool
unitPropagate = undefined

-- Return the next decision literal, or Nothing
-- if all literals have been assigned.
pickLiteral :: State SolverState (Maybe Lit)
pickLiteral = do
  SS {ass=a, n=n'} <- get
  return (find (\l -> val l a == U) [1..n'])

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