module Watching where

--  Eventually test with Strict variants
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Watching

type Watchlist = IntMap [Clause]

-- Get a list of clauses watching a literal.
clausesWatching :: Lit -> Watchlist -> [Clause]
clausesWatching = findWithDefault []



