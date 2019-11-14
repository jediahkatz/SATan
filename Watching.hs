module Watching where

--  Eventually test with Strict variants
import Data.IntMap.Lazy (IntMap, findWithDefault)
import qualified Data.IntMap.Lazy as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (find)
import Formula

type Watchlist = IntMap [Clause]
type WatchedLits = Map Clause (Lit, Lit)

-- Get a list of clauses watching a literal.
clausesWatching :: Lit -> Watchlist -> [Clause]
clausesWatching = findWithDefault []

-- Is this clause watching this literal?
isWatching :: Clause -> Lit -> WatchedLits -> Bool
isWatching c l w = case (Map.lookup c w) of
  Nothing       -> False
  Just (l1, l2) -> l == l1 || l == l2

-- Attempt to find a non-False literal in the
-- clause which is not already being watched.
-- Does the WatchedLits lookup only happen once due to laziness?
findNewWatchedLit :: Clause -> WatchedLits -> Assignment -> Maybe Lit
findNewWatchedLit c w a = find (\l -> not (isWatching c l w || val l a == F)) c


