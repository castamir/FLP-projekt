module MIN
(
) where

import qualified Data.Set as Set
import qualified Data.List as List
import DFA

type State      = String
type SuperState = Set.Set State
type Transition = Set.Set Char
type Rule       = (SuperState, Transition, SuperState)
type Rules 		= Set.Set Rule
type SimpleRule = (SuperState, Char, SuperState)

simpleRulesStep :: Rule -> [SimpleRule]
simpleRulesStep (p,a,q) =  map (\x -> (p,x,q)) $ Set.toList a

simpleRules :: Rules -> [SimpleRule]
simpleRules rls = concat $ map (simpleRulesStep) $ Set.toList rls

firstClass :: [SimpleRule] -> Set.Set SuperState -> [(Int, [SimpleRule])]
firstClass rls finish = [(1, fst x), (2, snd x)]  
	where 
		x = List.partition (\(p,_,_) -> Set.member p finish) rls

newClass :: [(Int, [SimpleRule])] -> [(Int, [SimpleRule])]
newClass cls = cls 

--newSingleClass :: (Int, [SimpleRule]) -> [(Int, [SimpleRule])] -> [(Int, SuperState)]
--newSingleClass cls classes = 
	--where
		--clsCol = filter (\(_,_,q) -> q) $ snd cls 
		--clsStates = map () clsCol

findClassId :: SuperState -> [(Int, [SimpleRule])] -> Int
findClassId a classes = List.find (\(x,y) -> elem a y) classes