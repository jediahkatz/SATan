module Basic where

import Formula
import Data.IntMap.Lazy(IntMap)
import qualified Data.IntMap.Lazy as Map
import Data.IntSet(IntSet)
import qualified Data.IntSet as Set


-- This defines a basic, inefficient SAT solver for testing purposes. It is
-- based on modified code from Sat.hs

-- What a variable must be set to in an assignment to be satisfied
--varSatCondition :: Lit -> Value
--varSatCondition l = if (isPos l) then T else F

-- Is a literal satisfied by a given assignment?
litSatisfied :: Assignment -> Lit -> Bool
litSatisfied a l = Map.member (var l) a && (isPos l == a Map.! (var l))

--Is the CNF formula satisfied by a given assignment?
satisfiedBy :: CNF -> Assignment -> Bool
satisfiedBy p a = all (any (litSatisfied a)) p

-- All the variables in a formula
varSet :: CNF -> IntSet
varSet = foldr inner Set.empty where
  inner :: Clause -> IntSet -> IntSet
  inner cl s = foldr (\l s' -> Set.insert (abs l) s') s cl

-- | All of the variables that appear anywhere in the formula, in sorted order
vars :: CNF -> [Var]
vars = Set.toList . varSet

-- All possible assignments of variables
makeValuations :: [Var] -> [Assignment]
makeValuations [] = [ Map.empty ]
makeValuations (v:vs) = map (Map.insert v True) result ++
                        map (Map.insert v False) result where
  result = makeValuations vs

-- Highly inefficient sat solver that looks at all possible assignments
sat0 :: Solver
sat0 c = foldr
         (\x acc -> if c `satisfiedBy` x then Just x else acc)
         Nothing
         (makeValuations $ vars c)

-- A slight improvement - simplify formula after choosing a value. This will
-- be the main reference implementation

instantiate :: CNF -> Var -> Value -> CNF
instantiate cs v b = foldr helper [] cs where
  helper :: Clause -> [Clause] -> [Clause]
  helper ls rest = case b of
                    T -> if v `elem` ls
                            then rest
                            else filter (/= -v) ls : rest
                    F -> if -v `elem` ls
                            then rest
                            else filter (/= v) ls : rest
                    U -> rest


-- Worth noting; this solver might only return a satisfying partial assignment.
sat1 :: Solver
sat1 = sat Map.empty where
  sat m c =
    if null c then Just m
    else
      if [] `elem` c then Nothing -- see if unsatisfiable
      else
        case vars c of
          [] -> Nothing
          (v:_) -> case sat m (instantiate c v T) of
                    Just m' -> Just (Map.insert v True m')
                    Nothing -> case sat m (instantiate c v F) of
                                Just m' -> Just (Map.insert v False m')
                                Nothing -> Nothing
                                
-- Naive DPLL, with unit propagation and pure literal elimination.
unitClauses :: CNF -> [Lit]
unitClauses f = [l | [l] <- f]

simplifyUnitClause :: CNF -> Maybe (CNF, Var, Bool)
simplifyUnitClause f = case unitClauses f of
  []  -> Nothing
  l:_ -> if l > 0 then
          Just (instantiate f l T, l, True)
         else
          Just (instantiate f (abs l) F, l, False)
  
  
pureLiterals :: CNF -> [(Var,Bool)]
pureLiterals f = [(v, if p == 1 then False else True) | v <- vars f, p <- [1, -1],
   all (notElem (p*v)) f ]

simplifyPureLiteral :: CNF -> Maybe (CNF, Var, Bool)
simplifyPureLiteral f = case pureLiterals f of
  []      -> Nothing
  (v,p):_ -> if p then
              Just (instantiate f v T, v, p)
             else
              Just (instantiate f v F, v, p)

dpll :: Solver
dpll = sat Map.empty where
  sat m f = 
    if null f then Just m
    else
      if [] `elem` f then Nothing
      else
        case vars f of
          []    -> Nothing
          (v:_) -> case simplifyUnitClause f of
            Just (f', v', p) -> case sat m f' of
              Nothing -> Nothing
              Just m' -> Just (Map.insert v' p m')
              
            Nothing -> case simplifyPureLiteral f of
              Just (f', v', p) -> case sat m f' of
                Nothing -> Nothing
                Just m' -> Just (Map.insert v' p m')
               
              Nothing -> case sat m (instantiate f v T) of
                Just m' -> Just (Map.insert v True m')
                Nothing -> case sat m (instantiate f v F) of
                  Just m' -> Just (Map.insert v False m')
                  Nothing -> Nothing