module Watching where

--  Eventually test with Strict variants
import Data.IntMap.Lazy (IntMap, findWithDefault)
import qualified Data.IntMap.Lazy as IntMap
import Formula

type Watchlist = IntMap [Clause]

-- Get a list of clauses watching a literal.
clausesWatching :: Lit -> Watchlist -> [Clause]
clausesWatching = findWithDefault []

-- Find a non-False literal in the clause.
findNonfalseLit :: Clause -> Assignment -> Lit


