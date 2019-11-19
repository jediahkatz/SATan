{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


import Basic
import Formula
import Watching
import Test.HUnit (Test(..), (~:), (~?=), runTestTT, assertBool)
import Test.QuickCheck
import Data.Maybe as Maybe
import Data.Map(fromList, empty)
import qualified Data.Map as Map
import Data.IntMap(fromList)
import qualified Data.IntMap as IntMap



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
genClause n = Disj <$> listOf (genLit n)

-- | Generate a random CNF with `n` distinct variables.
genCNF      :: Int -> Gen QCNF
genCNF    n = Conj <$> listOf (genClause n)

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