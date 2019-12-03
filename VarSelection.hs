import Control.Monad.State.Lazy(State, get, put)
import Data.IntMap.Lazy (IntMap, insertWith, findWithDefault)
import qualified Data.IntMap.Lazy as IntMap
import Data.Map.Lazy (Map, fromList, insert)
import qualified Data.Map.Lazy as Map
import Data.List (sortBy, nub)
import Formula

-- Based on a static ordering used by SATZOO.
heatOrder :: CNF -> [Var]
heatOrder f = 
  let va = varActivities f in
  let initHeats = clauseInitialHeats f va in
  let finHeats = clauseBoostedHeats f initHeats 8 in
  let list = [(s,c) | (c,s) <- Map.toList finHeats] in
  let sortedList = map snd (sortBy (flip compare) list) in
  -- Sort variables in the order they appear in hot clauses
  nub (map var (concat sortedList))
  
-- For each variable, its "activity" score is the 
-- sum of 1/2^|C| for each clause C it occurs in.
varActivities :: CNF -> IntMap Float
varActivities f = foldr (
  \c acc -> let len = length c in
    foldr (
    \l acc' -> insertWith (+) (var l) (1/(2^len)) acc'
    ) acc c
  ) IntMap.empty f

-- For each clause, its initial "heat" is the sum of the
-- activity of all its variables.
clauseInitialHeats :: CNF -> IntMap Float -> Map Clause Float
clauseInitialHeats f vActs = fromList [
  (c, sum [findWithDefault 0 (var l) vActs | l <- c]) | c <- f]
  
type OccurList = IntMap [Clause]
-- Get a list of clauses containing a variable.
clausesContaining :: Lit -> OccurList -> [Clause]
clausesContaining = (findWithDefault []) . var

-- Get map from literals to clauses containing it.
-- Could try substituting this for the Watchlist and see what happens. 
getOccurList :: CNF -> OccurList
getOccurList f = foldr (\c acc ->
    let addCToOccurList = (\mls -> case mls of
                              Nothing -> Just [c]
                              Just ls  -> Just (c:ls)) in
    foldr (\l acc' -> 
      IntMap.alter addCToOccurList (var l) acc'
    ) acc c
  ) IntMap.empty f

-- Increase the heat of clauses whose literals occur 
-- in other hot clauses. Repeat until "convergence" a la simulated annealing.
clauseBoostedHeats :: CNF -> Map Clause Float -> Float -> Map Clause Float
clauseBoostedHeats f initHeats iters = 
  -- iters: the number of iterations until convergence (typically <= 10)
  -- despite the types this should be an int
  -- For each literal l in each clause c, increase the heat of c
  -- based on the heat of each other clause that l appears in.
  cbhHelper iters initHeats 
  where
    decay = 1/iters
    occ = (getOccurList f)
    cbhHelper 0 heats = heats
    cbhHelper n heats = cbhHelper (n-1) (foldr 
      -- For each clause c...
      (\c acc ->
        -- For each literal l in c...
        foldr (\l acc' ->
            -- For each other clause c' containing l...
            foldr (\c' acc'' -> if c == c' then acc'' else
                -- Increase heat(c) by heat(c') * decay
                let incr = (Map.findWithDefault 0 c' acc'') * decay in
                Map.adjust (+(incr)) c acc''
            ) acc' (clausesContaining l occ)
        ) acc c
      ) heats f)

-- Abandoned for now.
-- -- Keys are (isUnassigned, score).
-- type DynamicScores = Map (Bool, Int) Var

-- -- Goal: whenever unit propagation from clause C causes a conflict,
-- -- increase the score of all variables in C.

-- -- Pick the unassigned variable with the highest score 
-- -- and mark it as assigned in the score heap.
-- -- If all variables are assigned, returns Nothing.
-- pickVariableVSIDS :: DynamicScores -> Maybe (Var, DynamicScores)
-- pickVariableVSIDS scores = do
  -- (((isUnassigned, s), v), scores')  <- maxViewWithKey scores
  -- case isUnassigned of
    -- False -> Nothing
    -- True  -> Just (v, insert (False, s) v scores')

-- -- Increment the score of a variable in the heap.
-- incrementScoreVSIDS :: Var -> State DynamicScores Bool
-- incrementScoreVSIDS = undefined

-- -- Halve the scores of all variables in the heap.
-- decreaseScoresVSIDS :: State DynamicScores Bool
-- decreaseScoresVSIDS = undefined

-- -- Update the heap so that the states of variables match the assignment.
-- updateStatesVSIDS :: Assignment -> State DynamicScores Bool
-- updateStatesVSIDS = undefined