type Var = Int
type Lit = Int

-- Get the variable of a literal.
v :: Lit -> Var
v = abs

-- Whether a literal is positive.
isPos :: Lit -> Bool
isPos = (>0)

-- Negate a literal.
neg :: Lit -> Lit
neg = negate

type Clause = [Lit]
type CNF = [Clause]