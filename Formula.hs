module Formula where

--  Eventually test with Strict variants
import Data.IntMap.Lazy (IntMap, findWithDefault, insert)
--import qualified Data.IntMap.Lazy as IntMap
import Control.Monad.State.Lazy(State, get, put)
--import State (State)
import Data.List (find)

type Var = Int
type Lit = Int

-- Get the variable of a literal.
var :: Lit -> Var
var = abs

-- Whether a literal is positive.
isPos :: Lit -> Bool
isPos = (>0)

-- Negate a literal.
neg :: Lit -> Lit
neg = negate

type Clause = [Lit]
type CNF = [Clause]

type Assignment = IntMap Value

type Solver = CNF -> Maybe Assignment

-- True, False, Undefined
data Value = T | F | U
  deriving (Eq, Show)
  
-- Get the value of a literal in a given assignment.
val :: Lit -> Assignment -> Value
val = findWithDefault U

-- Solver state containing number of variables, current assignment
-- and propagation queue (data structures can change).
data SolverState = SS {
  n :: Int,
  ass :: Assignment,
  propQ :: [Lit]
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

dpllHelper :: State SolverState ()
dpllHelper = undefined

--dpll = do
--  s <- unitPropagate
--  x = pickLiteral
--  dpll (assume x)
--  dpll (assume not x)
--  unsat
  
