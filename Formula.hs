module Formula where

--  Eventually test with Strict variants
import Data.IntMap.Lazy (IntMap, findWithDefault)
import qualified Data.IntMap.Lazy as IntMap
import State (State)

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

-- True, False, Undefined
data Value = T | F | U
  deriving (Eq, Show)
  
-- Get the value of a literal in a given assignment.
val :: Lit -> Assignment -> Value
val = findWithDefault U

-- Solver state containing current assignment
-- and propagation queue (data structures can change).
data SolverState = SS {
  ass :: Assignment,
  propQ :: [Lit]
}

-- Attempt to assign given literal to True.
-- If it was already False, return False;
-- Otherwise return True and add its negation
-- to the end of the propQ if it was unassigned.
assume :: Lit -> State SolverState Bool
assume = undefined

-- Repeatedly attempt unit propagation until the
-- propagation queue is empty, updating the SolverState.
-- If at any point, we assume a conflict, then
-- unitPropagate returns False. Otherwise it returns True.
unitPropagate :: State SolverState Bool
unitPropagate = undefined

-- Return the next decision literal, or Nothing
-- if all literals have been assigned.
pickLiteral :: SolverState -> Maybe Lit

--dpll = do
--  s <- unitPropagate
--  x = selectVariable
--  dpll (assume x)
--  dpll (assume not x)
--  unsat
  
