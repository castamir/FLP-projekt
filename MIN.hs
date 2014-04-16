import qualified Data.Set as Set
import qualified Data.List as List
import DFA

type State      = String
type SuperState = Set.Set State
type Transition = Set.Set Char
type Rule       = (SuperState, Transition, SuperState)
type SimpleRule = (SuperState, Char, SuperState)

makeSimpleRules :: Set.Set Rule -> [SimpleRule]
makeSimpleRules rls = concat $ map (simpleRulesStep) $ Set.toList rls
    where
      simpleRulesStep (p,a,q) = map (\x -> (p,x,q)) $ Set.toList a

firstClass :: [SimpleRule] -> Set.Set SuperState -> [(Int, [SimpleRule])]
firstClass rls finish = [(1, fst x), (2, snd x)]  
    where
      x = List.partition (\(p,_,_) -> Set.member p finish) rls 

classDelta :: (Int, [SimpleRule]) -> Char -> [SuperState]
classDelta cls sym = map (\(_,_,q) -> q) $ filter (\(p,a,q) -> a == sym) $ snd cls

classId :: [(Int, [SimpleRule])] -> SuperState -> Int
classId classes s = fst $ head $ filter (\x -> elem s $ snd x) $ map (\(i,x) -> (i, srcStates x)) classes
    where
      srcStates rls = map (\(p,_,_) -> p) rls

splitClass :: (Int, [SimpleRule]) -> [[SimpleRule]]
splitClass cls = 
