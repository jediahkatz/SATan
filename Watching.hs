module Watching where

--  Eventually test with Strict variants
import Data.IntMap.Lazy (IntMap, findWithDefault, lookup)
import qualified Data.IntMap.Lazy as IntMap
import Data.Map (Map, lookup)
import qualified Data.Map as Map
import Data.List (find)
import Formula

type Watchlist = IntMap [Clause]

type WatchedLitsMap = Map Clause (Lit, Lit)

-- Get the literals watched by a clause.
watchedLits :: Clause -> WatchedLitsMap -> Either Lit (Lit, Lit)
watchedLits [] _ = Left 0
watchedLits [l] _ = Left l
watchedLits c wlm = case (Map.lookup c wlm) of
  Nothing -> Left 0
  Just pr -> Right pr

-- Get a list of clauses watching a literal.
clausesWatching :: Lit -> Watchlist -> [Clause]
clausesWatching = findWithDefault []

-- Is this clause watching this literal?
isWatching :: Clause -> Lit -> WatchedLitsMap -> Bool
isWatching c l w = case (watchedLits c w) of
  Left l'        -> l == l'
  Right (l1, l2) -> l == l1 || l == l2
  
satisfiedByWatchedLit :: Clause -> WatchedLitsMap -> Assignment -> Bool
satisfiedByWatchedLit [] _ _ = False
satisfiedByWatchedLit c wlm a = case (watchedLits c wlm) of
  Left l          -> val l a == T
  Right (l1, l2)  -> val l1 a == T || val l2 a == T

-- Attempt to find a non-False literal in the
-- clause which is not already being watched.
-- Does the WatchedLitsMap lookup only happen once due to laziness?
findNewWatchedLit :: Clause -> WatchedLitsMap -> Assignment -> Maybe Lit
findNewWatchedLit c wlm a = find (\l -> not (isWatching c l wlm || val l a == F)) c




