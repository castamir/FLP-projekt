module DFA
( DFA(..)
--, nfa2dfa
)where

import qualified Data.Set as Set
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

notDisjoint :: Ord a => Set.Set a -> Set.Set a -> Bool
notDisjoint s1 s2 = not $ Set.null $ Set.intersection s1 s2

reflxClosure :: Ord a => Set.Set (a,a) -> Set.Set (a,a)
reflxClosure s = Set.union s $ Set.fromList [(fst a, fst a) | a <- Set.toList s]

transClosure :: Ord a => Set.Set (a,a) -> Set.Set (a,a)
transClosure s = if s == trans
                  then s
                  else transClosure trans

    where trans = Set.union s $ Set.fromList [(fst a, snd b) | a <- Set.toList s, b <- Set.toList s, snd a == fst b]

nfaEpsClosure :: NFA -> NFA
nfaEpsClosure nfa = NFA { NFA.name   = NFA.name nfa
                        , NFA.states = NFA.states nfa
                        , NFA.alph   = NFA.alph nfa
                        , NFA.rules  = Set.union (NFA.rules nfa) $ Set.map (\(p,q) -> (p,Nothing,q)) $ transClosure $ reflxClosure epsTransition
                        , NFA.start  = NFA.start nfa
                        , NFA.finish = NFA.finish nfa
                        }

    where epsTransition = Set.fromList [(p,q) | (p,a,q) <- Set.toList (NFA.rules nfa), a == Nothing]

epsClosureState :: NFA -> State -> Set.Set State
epsClosureState nfa s = Set.map (\(p,a,q) -> q) $ Set.filter (\(p,a,q) -> p == s && a == Nothing) (NFA.rules nfa)

epsClosureSet :: NFA -> Set.Set State -> Set.Set State
epsClosureSet nfa s = Set.foldl Set.union Set.empty $ Set.map (epsClosureState nfa) s

deltaSupState :: NFA -> SuperState -> Char -> SuperState
deltaSupState nfa s x = Set.map (\(p,a,q) -> q) $ Set.filter (\(p,a,q) -> Set.member p s && Set.member x a) $ Set.filter (\(_,a,_) -> a /= Nothing) (NFA.rules nfa)

genStatesStep :: NFA -> SuperState -> Set.Set SuperState
genStatesStep nfa ss = Set.map (epsClosureSet nfa) $ Set.map (deltaSupState nfa ss) (NFA.alph nfa)

genStates :: NFA -> Set.Set SuperState -> Set.Set SuperState
genStates nfa ss = Set.foldl Set.union ss $ Set.map (genStatesStep nfa) ss

--nfa2dfa :: NFA -> DFA
--nfa2dfa  nfa = nfa2dfa' $ nfaEpsClosure nfa
--nfa2dfa' nfa = DFA { DFA.name   = NFA.name nfa
--                   , DFA.start  = newStart
--                   , DFA.states = newStates
--                   }

--    where epsClosureState s = Set.map (\(p,a,q) -> q) $ Set.filter (\(p,a,q) -> p == s && a == Nothing) (NFA.rules nfa)
--          epsClosureSet   s = Set.foldl Set.union Set.empty $ Set.map epsClosureState s
--          deltaSupState s x = Set.map (\(p,a,q) -> q) $ Set.filter (\(p,a,q) -> Set.member p s && a == x) (NFA.rules nfa)

--          genStates s = if s == genStates'
--                          then s
--                          else genStates genStates'
              
--              where genStates' = Set.union s $ 
          
--          newStart  = epsClosureState NFA.start nfa
--          newStates = genStates (Set.singleton newStart)

test_rel = Set.fromList [(1,2),(2,3),(3,4)]

test_nfa_a = NFA { NFA.name   = "a"
                 , NFA.states = Set.fromList ["a1", "a2"]
                 , NFA.alph   = Set.singleton 'a'
                 , NFA.rules  = Set.fromList [("a1", Just (Set.singleton 'a'), "a2")]
                 , NFA.start  = "a1"
                 , NFA.finish = Set.fromList ["a2"]
                 }

test_nfa_b = NFA { NFA.name   = "b"
                 , NFA.states = Set.fromList ["b1", "b2"]
                 , NFA.alph   = Set.singleton 'b'
                 , NFA.rules  = Set.fromList [("b1", Just (Set.singleton 'b'), "b2")]
                 , NFA.start  = "b1"
                 , NFA.finish = Set.fromList ["b2"]
                 }
