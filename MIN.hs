------------------------------------------------------
-- Project: Simple Grep
-- Authors:
--   xsurov03 - Marek Surovic
--   xstodu05 - Petr Stodulka
--   xpavlu06 - Igor Pavlu
--   xpauli00 - Miroslav Paulik
------------------------------------------------------

module MIN (dfa2mfa) where

import qualified Data.Set as Set
import qualified Data.List as List
import DFA

type State      = String
type SuperState = Set.Set State
type Transition = Set.Set Char
type Rule       = (SuperState, Transition, SuperState)
type SimpleRule = (SuperState, Char, SuperState)

-------------------------------------------------------------------------------------
-- make list of simple rules (p,a) -> q from given set of rules  return list of simlpe rule
makeSimpleRules :: Set.Set Rule -> [SimpleRule]
makeSimpleRules rls = concat $ map (simpleRulesStep) $ Set.toList rls
    where
      simpleRulesStep (p,a,q) = map (\x -> (p,x,q)) $ Set.toList a

-------------------------------------------------------------------------------------
-- make initial class of rules to end and other return class 
initClasses :: Set.Set SuperState -> [SimpleRule] -> [(Int, [SimpleRule])]
initClasses finish rls = [(1, fst x), (2, snd x)]  
    where
      x = List.partition (\(p,_,_) -> Set.member p finish) rls 

-------------------------------------------------------------------------------------
-- from given class and state return id of class
classId :: [(Int, [SimpleRule])] -> SuperState -> Int
classId classes s = fst $ head $ filter (\x -> elem s $ snd x) $ map (\(i,x) -> (i, srcStates x)) classes
    where
      srcStates rls = map (\(p,_,_) -> p) rls

-------------------------------------------------------------------------------------
-- split class based on transitions between classes return list of new classes
splitClass :: [(Int, [SimpleRule])] -> (Int, [SimpleRule]) -> [Char] -> [(Int, [SimpleRule])]
splitClass classes cls []     = classes
splitClass classes cls (x:xs) = if length newClasses /= length classes
                                  then newClasses
                                  else splitClass classes cls xs
    where
      newClasses = splitClassStep classes cls x

-------------------------------------------------------------------------------------
-- make one step of spliting class based on actual class and given symbol return list of classes
splitClassStep :: [(Int, [SimpleRule])] -> (Int, [SimpleRule]) -> Char -> [(Int, [SimpleRule])]
splitClassStep classes cls sym = if length groups > 1
                                  then zip [1..] $ (List.delete (snd cls) $ map (snd) classes) ++ groupedRules
                                  else classes
    where
      rightSides     = map (\(_,_,q) -> q) $ filter (\(_,a,_) -> a == sym) $ snd cls
      leftSides      = map (\(p,_,_) -> p) $ filter (\(_,a,_) -> a == sym) $ snd cls
      pairs          = zip leftSides $ zip rightSides $ map (classId classes) rightSides
      groups         = map (map fst) $ List.groupBy (\(_,x) (_,y) -> snd x == snd y) $ List.sortBy (\(_,x) (_,y) -> compare (snd x) (snd y)) pairs
      groupedRules   = map (groupRules) groups
      groupRules grp = filter (\(p,_,_) -> elem p grp) $ snd cls

-------------------------------------------------------------------------------------
-- split all classes based on classes and alphabet return list of new classes
splitAll :: [(Int, [SimpleRule])] -> [Char] -> [(Int, [SimpleRule])] -> [(Int, [SimpleRule])]
splitAll classes alp []     = classes
splitAll classes alp (x:xs) = if length newClasses == length classes
                                then splitAll classes alp xs
                                else splitAll newClasses alp newClasses
    where
      newClasses = splitClass classes x alp

-------------------------------------------------------------------------------------
-- generate new states from classes return new set of states 
genStates :: [(Int, [SimpleRule])] -> Set.Set SuperState
genStates classes = Set.fromList $ map (Set.singleton . show) $ map fst classes

-------------------------------------------------------------------------------------
-- generate rules from classes return new set of rules
genRules :: [(Int, [SimpleRule])] -> Set.Set Rule
genRules classes = Set.fromList $ zipWith (\a (p,_,q) -> (p,a,q)) symbols $ map head groupedRules
    where
      symbols            = map Set.fromList $ map rules2symbols groupedRules
      rules2symbols x    = map (\(_,a,_) -> a) x  
      groupedRules       = List.groupBy (\(p1,_,q1) (p2,_,q2) -> p1 == p2 && q1 == q2) renamedRules
      renamedRules       = concatMap (genRules') classes
      genRules' (i, rls) = List.nub $ map (\(p,a,q) -> (Set.singleton $ show i, a, Set.singleton $ show $ classId classes q)) rls

-------------------------------------------------------------------------------------
-- generate fineal states based on classes and original finish states of DFA returns set of new final states
genFinals :: [(Int, [SimpleRule])] -> Set.Set SuperState -> Set.Set SuperState
genFinals classes finals = Set.map (Set.singleton . show . classId classes) finals

-------------------------------------------------------------------------------------
-- provides minimaization of given DFA returns minimal DFA
dfa2mfa :: DFA -> DFA
dfa2mfa dfa = DFA { DFA.name    = "MIN_" ++ DFA.name dfa
                  , DFA.states  = genStates classes
                  , DFA.alph    = DFA.alph dfa
                  , DFA.rules   = genRules classes
                  , DFA.start   = Set.singleton $ show $ classId classes $ DFA.start dfa
                  , DFA.finish  = genFinals classes $ DFA.finish dfa 
                  }
    where
      classes = splitAll init (Set.toList $ DFA.alph dfa) init
      init    = initClasses (DFA.finish dfa) $ makeSimpleRules $ DFA.rules dfa
