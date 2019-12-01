{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


import Basic
import Formula
import Watching
import Test.HUnit (Test(..), (~:), (~?=), runTestTT, assertBool)
import Test.QuickCheck
import Data.Maybe as Maybe
import Data.Map(fromList, empty)
import qualified Data.Map as Map
import Data.IntMap(fromList, toList)
import qualified Data.IntMap as IntMap
import Solver(solve, SolverState(SS), s_wl, s_wlm, assume)
import Control.Monad.State.Lazy(State, execState)
import Data.List(nub)



-- Arbitrary instance for formulas
newtype QCNF = Conj { clauses :: [QClause] } deriving (Eq, Ord, Show)
newtype QClause = Disj { lits ::  Clause } deriving (Eq, Ord, Show)


allVars :: [Var]
allVars = [1 ..]

genVar      :: Int -> Gen Var
genVar    n = elements (take (abs n + 1) allVars)

genLit      :: Int -> Gen Lit
genLit    n = do
  n' <- genVar n
  b <- (choose (0, 1) :: Gen Int)
  if b == 0 then return (-n') else return n'

genClause   :: Int -> Gen QClause
genClause n = Disj <$> (do
  l <- listOf (genLit n)
  return (nub l))

-- | Generate a random CNF with `n` distinct variables.
genCNF      :: Int -> Gen QCNF
genCNF    n = Conj <$> (do
  l <- listOf (fmap unwrapClause (genClause n))
  return (fmap Disj (nub (filter (/= []) l))))

defaultNumVariables :: Int
defaultNumVariables = 5

shrinkValue :: Value -> [Value]
shrinkValue T = [F]
shrinkValue _ = []

shrinkVar :: Var -> [Var]
shrinkVar v | v == 1 = []
            | otherwise = [1 .. pred v]

shrinkLit :: Lit -> [Lit]
shrinkLit l = [l, -l]

instance Arbitrary QClause where
   arbitrary = genClause defaultNumVariables
   shrink (Disj l) = [Disj l' | l' <- shrink l]

instance Arbitrary QCNF where
   arbitrary = fmap Conj arbitrary
   shrink (Conj x) = [Conj x' | x' <- shrink x]

unwrapClause :: QClause -> Clause
unwrapClause (Disj x) = x

unwrapCNF :: QCNF -> CNF
unwrapCNF (Conj x) = fmap unwrapClause x

prop_makeValuations :: QCNF -> Bool
prop_makeValuations p = length valuations == 2 ^ length ss
                     && allElementsDistinct valuations
  where
    valuations = makeValuations ss
    ss = vars (unwrapCNF p)

allElementsDistinct :: Eq a => [a] -> Bool
allElementsDistinct []     = True
allElementsDistinct (x:xs) = x `notElem` xs
                          && allElementsDistinct xs

prop_satResultSound :: Solver -> Int -> Property
prop_satResultSound solver i =
    forAll (genCNF i) $ \p -> case solver (unwrapCNF p) of
        Just a  -> (unwrapCNF p) `satisfiedBy` a
        Nothing -> True

unsatisfiable :: QCNF -> Bool
unsatisfiable p = let p' = unwrapCNF p in
   all (\a -> not (p' `satisfiedBy` a))
    (makeValuations (vars p'))

prop_satResult :: Solver -> QCNF -> Bool
prop_satResult solver p = let p' = unwrapCNF p in
  case solver p' of
    Just a  -> p' `satisfiedBy` a
    Nothing -> unsatisfiable p

-- TODO: discards many tests
prop_instantiate :: QCNF -> Var -> Property
prop_instantiate p v = let p' = unwrapCNF p in
  v `elem` vars p' ==>
  isNothing (sat0 p') ==
  (isNothing(sat0 (instantiate p' v T)) &&
    isNothing (sat0 (instantiate p' v F)))

prop_sat1 :: QCNF -> Bool
prop_sat1 s = let s' = unwrapCNF s in isJust (sat1 s') == isJust (sat0 s')

-- Tests for functions in Formula.hs

--tests for val

sampleAssignment' :: Assignment
sampleAssignment' = IntMap.fromList [(1, True), (2, False), (3, True)]

valTest :: Test
valTest = TestList [
  val 1 sampleAssignment' ~?= T,
  val (-1) sampleAssignment' ~?= F,
  val 2 sampleAssignment' ~?= F,
  val (-2) sampleAssignment' ~?= T,
  val 4 sampleAssignment' ~?= U,
  val (-4) sampleAssignment' ~?= U]


assignTest :: Test
assignTest = TestList [
  val 1 (assign 1 True sampleAssignment') ~?= T,
  val 2 (assign 2 True sampleAssignment') ~?= T,
  val 3 (assign 3 False sampleAssignment') ~?= F,
  val 4 (assign 1 False sampleAssignment') ~?= U,
  val 2 (assign (-2) True sampleAssignment') ~?= F]

-- Tests for functions in Watching.hs

sampleClause1 :: Clause
sampleClause1 = [1]

sampleClause2 :: Clause
sampleClause2 = [1, 2, 3]

sampleClause3 :: Clause
sampleClause3 = [1, -2, 3, -5, 4, 1]

sampleMap :: WatchedLitsMap
sampleMap = Map.fromList ([(sampleClause2, (1, 2)),
  (sampleClause3, (-2, 3))]) 

-- watchedLits
watchedLitsTest :: Test
watchedLitsTest = TestList [
  watchedLits sampleClause2 sampleMap ~?= Right (1,2),
  watchedLits sampleClause3 sampleMap ~?= Right (-2,3),
  watchedLits sampleClause1 empty ~?= Left 1,
  watchedLits [] sampleMap ~?= Left 0,
  watchedLits [8, 6, 7] sampleMap ~?= Left 0 ]

-- isWatching
isWatchingTest :: Test
isWatchingTest = TestList [
  isWatching sampleClause1 1 empty ~?= True,
  isWatching sampleClause1 2 empty ~?= False,
  isWatching sampleClause2 1 sampleMap ~?= True,
  isWatching sampleClause2 2 sampleMap ~?= True,
  isWatching sampleClause2 3 sampleMap ~?= False,
  isWatching [] 0 sampleMap ~?= False]

sampleAssignment :: Assignment
sampleAssignment = IntMap.fromList [(1, True), (2, True)]

-- satisfiedByWatchedLit
satisfiedByWatchedLitTest :: Test
satisfiedByWatchedLitTest = TestList [
  satisfiedByWatchedLit sampleClause1 sampleMap sampleAssignment ~?= True,
  satisfiedByWatchedLit sampleClause2 sampleMap sampleAssignment ~?= True,
  satisfiedByWatchedLit sampleClause3 sampleMap sampleAssignment ~?= False,
  satisfiedByWatchedLit [] sampleMap sampleAssignment ~?= False]

--findNewWatchedLit
findNewWatchedLitTest :: Test
findNewWatchedLitTest = TestList [
  findNewWatchedLit sampleClause1 sampleMap sampleAssignment ~?= Nothing,
  findNewWatchedLit sampleClause2 sampleMap sampleAssignment ~?= Just 3,
  findNewWatchedLit sampleClause3 sampleMap sampleAssignment ~?= Just 1]

sampleWatchList :: Watchlist
sampleWatchList = IntMap.fromList [(1, [sampleClause1]), (2, [sampleClause1,
  sampleClause2])]

-- clausesWatching
clausesWatchingTest :: Test
clausesWatchingTest = TestList [
  clausesWatching 1 sampleWatchList ~?= [sampleClause1],
  clausesWatching 2 sampleWatchList ~?= [sampleClause1, sampleClause2],
  clausesWatching 3 sampleWatchList ~?= []]

-- Tests for solver

-- Invariants of the solver state

-- 1. The watchedList and WatchedListsMap must agree, ie, if (l, c) is in
--    the watchedList, then l must be in the list mapped to by c in the 
--    watchedLitsMap

-- We need a helper function

-- Convert the watchList to a list of (Lit, Clause) pairs
getLitPairs :: Watchlist -> [(Lit, Clause)]
getLitPairs m = foldr inner [] (IntMap.toList m) where
  inner :: (Lit, [Clause]) -> [(Lit, Clause)] -> [(Lit, Clause)]
  inner (l, cs) acc = (map (\x -> (l, x)) cs) ++ acc

getLitPairsTest :: Test 
getLitPairsTest = TestList [
  getLitPairs (IntMap.fromList [(1, [sampleClause1, sampleClause2]), (3, [sampleClause3])])
  ~?= [(1, sampleClause1), (1, sampleClause2), (3, sampleClause3)],
  getLitPairs IntMap.empty ~?= [],
  getLitPairs (IntMap.fromList [(1, [sampleClause1, sampleClause2]), (3, [sampleClause1])])
  ~?= [(1, sampleClause1), (1, sampleClause2), (3, sampleClause1)]]

-- Now we want to make sure that for every (Lit, Clause) pair in the watchedList,
-- isWatching returns true (with the watchedListsMap)
mapsConsistent :: Watchlist -> WatchedLitsMap -> Bool
mapsConsistent m1 m2 = all (\(l,c) -> isWatching c l m2) (getLitPairs m1)

stateMapsConsistent :: SolverState -> Bool
stateMapsConsistent SS{s_wl = m1, s_wlm = m2} = mapsConsistent m1 m2 

-- Now, we can use this to show that the state maps are consistent after each
-- intermediate step, but we need to know the starting state. Not sure what the
-- best way is

prop_assumeConsistent :: Lit -> Bool
prop_assumeConsistent l = undefined -- stateMapsConsistent (execState (assume l))

prop_solve_equiv :: QCNF -> Bool
prop_solve_equiv s = let s' = unwrapCNF s in isJust (solve s') == isJust (sat1 s')

-- Some unit tests for the solver
solverTest :: Test
solverTest = TestList [
  solve ([[-1]]) ~?= Just (IntMap.fromList [(1, False)]),
  solve [[1], [2]] ~?= Just (IntMap.fromList [(1, True), (2, True)]),
  solve [[1, 2], [-1]] ~?= Just (IntMap.fromList [(1, False), (2, True)]),
  solve [[1], [-1]] ~?= Nothing,
  solve [[1, 2], [-1, 3, 4], [-1, -3, 4], [-1, -3, -4], [-1, 3, -4]] ~?= Just (IntMap.fromList [(1, False), (2, True), (3, True), (4, True)]),
  solve [[1,2], [-1, -2], [1, -2], [2, -1]] ~?= Nothing]