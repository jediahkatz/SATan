module Formula where

import Data.IntMap.Lazy (IntMap, findWithDefault, lookup)
import qualified Data.IntMap.Lazy as IntMap

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

-- Maps VARIABLES (not Literals!) to True/False
type Assignment = IntMap Bool

type Solver = CNF -> Maybe Assignment

-- True, False, Undefined
data Value = T | F | U
  deriving (Eq, Show)
  
-- Get the value of a literal in a given assignment.
val :: Lit -> Assignment -> Value
val l a = if isPos l 
  then
    case (IntMap.lookup l a) of
      Just True   -> T
      Just False  -> F
      Nothing     -> U
  else
    case (IntMap.lookup (neg l) a) of
      Just True   -> F
      Just False  -> T
      Nothing     -> U
      
-- Set the value of a LITERAL (not a variable) in the assignment
assign :: Lit -> Bool -> Assignment -> Assignment
assign = undefined