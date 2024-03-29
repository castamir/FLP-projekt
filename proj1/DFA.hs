------------------------------------------------------
-- Project: Simple Grep
-- Authors:
--   xsurov03 - Marek Surovic
--   xstodu05 - Petr Stodulka
--   xpavlu06 - Igor Pavlu
--   xpauli00 - Miroslav Paulik
------------------------------------------------------

module DFA
( DFA(..)
, nfa2dfa
)where

import qualified Data.Set  as Set
import qualified Data.List as List
import NFA

type State      = String
type SuperState = Set.Set State
type Transition = Set.Set Char
type Rule       = (SuperState, Transition, SuperState)

data DFA = DFA { name   :: String
               , states :: Set.Set SuperState
               , alph   :: Set.Set Char
               , rules  :: Set.Set Rule
               , start  :: SuperState
               , finish :: Set.Set SuperState
               } deriving (Show)

-------------------------------------------------------------------------------------
-- if two sets are disjoint return true, false otherwise
notDisjoint :: Ord a => Set.Set a -> Set.Set a -> Bool
notDisjoint s1 s2 = not $ Set.null $ Set.intersection s1 s2

-------------------------------------------------------------------------------------
-- find reflexive closure of given set
-- Nepouzivat! Vraci nekompletni reflexivni uzaver !
reflxClosure :: Ord a => Set.Set (a,a) -> Set.Set (a,a)
reflxClosure s = Set.union s $ Set.fromList [(fst a, fst a) | a <- Set.toList s]

-------------------------------------------------------------------------------------
-- find transitive closure of given set
transClosure :: Ord a => Set.Set (a,a) -> Set.Set (a,a)
transClosure s = if s == trans
                  then s
                  else transClosure trans

    where trans = Set.union s $ Set.fromList [(fst a, snd b) | a <- Set.toList s, b <- Set.toList s, snd a == fst b]

-------------------------------------------------------------------------------------
-- take NFA and return NFA without epsilon rules
nfaEpsClosure :: NFA -> NFA
nfaEpsClosure nfa = NFA { NFA.name   = NFA.name nfa
                        , NFA.states = NFA.states nfa
                        , NFA.alph   = NFA.alph nfa
                        , NFA.rules  = Set.union (NFA.rules nfa) $ Set.map (\(p,q) -> (p,Set.empty,q)) $ transClosure epsRelation
                        , NFA.start  = NFA.start nfa
                        , NFA.finish = NFA.finish nfa
                        }

    where epsRelation = Set.union epsLoop epsRule
          epsLoop = Set.map (\s -> (s,s)) $ NFA.states nfa
          epsRule = Set.fromList [(p,q) | (p,a,q) <- Set.toList (NFA.rules nfa), Set.null a]

-------------------------------------------------------------------------------------
-- find states of right side of epsilon rule from given state s of NFA return set states
epsClosureState :: NFA -> State -> Set.Set State
epsClosureState nfa s = Set.map (\(p,a,q) -> q) $ Set.filter (\(p,a,q) -> p == s && Set.null a) (NFA.rules nfa)

-------------------------------------------------------------------------------------
-- make epsilon closure set of states from NFA based on start state s return this set
epsClosureSet :: NFA -> Set.Set State -> Set.Set State
epsClosureSet nfa s = Set.foldr Set.union Set.empty $ Set.map (epsClosureState nfa) s

-------------------------------------------------------------------------------------
-- from given NFA start state and char return state on right side of rule 
deltaSupState :: NFA -> SuperState -> Char -> SuperState
deltaSupState nfa s x = Set.map (\(p,a,q) -> q) $ Set.filter (\(p,a,q) -> Set.member p s && Set.member x a) (NFA.rules nfa)

-------------------------------------------------------------------------------------
-- generate new substate from NFA and start state based on delta function return new substate
genStatesStep :: NFA -> SuperState -> Set.Set SuperState
genStatesStep nfa ss = Set.map (epsClosureSet nfa) $ Set.map (deltaSupState nfa ss) (NFA.alph nfa)

-------------------------------------------------------------------------------------
-- generate new state from NFA and start state  return new state
genStates :: NFA -> Set.Set SuperState -> Set.Set SuperState
genStates nfa ss = Set.foldr Set.union ss $ Set.map (genStatesStep nfa) ss

-------------------------------------------------------------------------------------
-- generate new states of NFA from state ss return new state 
genStatesAll :: NFA -> Set.Set SuperState -> Set.Set SuperState
genStatesAll nfa ss = if ss == new_ss
                        then ss
                        else genStatesAll nfa new_ss

    where new_ss = Set.union ss $ genStates nfa ss

-------------------------------------------------------------------------------------
-- generate new set of rules form input NFA and state return Set of rule
genRulesStep :: NFA -> SuperState -> Set.Set Rule
genRulesStep nfa ss = Set.fromList $ zipWith (\(p,_,q) s -> (p,s,q)) (map head groupedRules) $ map (Set.fromList . rules2symbols) groupedRules
    
    where newRules      = Set.map (\x -> (ss, x, epsClosureSet nfa $ deltaSupState nfa ss x)) (NFA.alph nfa)
          groupedRules  = List.groupBy (\(_,_,q1) (_,_,q2) -> q1 == q2) $ List.sortBy (\(_,_,q1) (_,_,q2) -> compare q1 q2) $ Set.toList newRules
          rules2symbols = map (\(_,a,_) -> a)

-------------------------------------------------------------------------------------
-- from input NFA and new set of states create new set of rules - return set of rule
genRules :: NFA -> Set.Set SuperState -> Set.Set Rule
genRules nfa ss = Set.foldr Set.union Set.empty $ Set.map (genRulesStep nfa) ss

-------------------------------------------------------------------------------------
-- create DFA from NFA given on input return DFA
nfa2dfa :: NFA -> DFA
nfa2dfa  nfa = nfa2dfa' $ nfaEpsClosure nfa
nfa2dfa' nfa = DFA { DFA.name   = NFA.name nfa
                   , DFA.start  = newStart
                   , DFA.states = newStates
                   , DFA.rules  = newRules
                   , DFA.alph   = NFA.alph nfa
                   , DFA.finish = newFinish
                   }

    where newStart  = epsClosureState nfa $ NFA.start nfa
          newStates = genStatesAll nfa $ Set.singleton newStart
          newRules  = genRules nfa newStates
          newFinish = Set.filter (notDisjoint $ NFA.finish nfa) newStates

