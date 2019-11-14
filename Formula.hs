module Formula where

--  Eventually test with Strict variants
import Data.IntMap.Lazy (IntMap, findWithDefault)
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

type Assignment = IntMap Value

-- True, False, Undefined
data Value = T | F | U
  deriving (Eq, Show)
  
-- Get the value of a literal in a given assignment.
val :: Lit -> Assignment -> Value
val = findWithDefault U