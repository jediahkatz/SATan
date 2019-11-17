{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


import Basic
import Formula
import Test.HUnit (Test(..), (~:), (~?=), runTestTT, assertBool)
import Test.QuickCheck
import Data.Maybe as Maybe


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