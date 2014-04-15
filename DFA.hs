module DFA
( DFA(..)
, nfa2dfa
, test_nfa
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

notDisjoint :: Ord a => Set.Set a -> Set.Set a -> Bool
notDisjoint s1 s2 = not $ Set.null $ Set.intersection s1 s2

-- Nepouzivat! Vraci nekompletni reflexivni uzaver !
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
                        , NFA.rules  = Set.union (NFA.rules nfa) $ Set.map (\(p,q) -> (p,Set.empty,q)) $ transClosure epsRelation
                        , NFA.start  = NFA.start nfa
                        , NFA.finish = NFA.finish nfa
                        }

    where epsRelation = Set.union epsLoop epsRule
          epsLoop = Set.map (\s -> (s,s)) $ NFA.states nfa
          epsRule = Set.fromList [(p,q) | (p,a,q) <- Set.toList (NFA.rules nfa), Set.null a]

epsClosureState :: NFA -> State -> Set.Set State
epsClosureState nfa s = Set.map (\(p,a,q) -> q) $ Set.filter (\(p,a,q) -> p == s && Set.null a) (NFA.rules nfa)

epsClosureSet :: NFA -> Set.Set State -> Set.Set State
epsClosureSet nfa s = Set.foldl Set.union Set.empty $ Set.map (epsClosureState nfa) s

deltaSupState :: NFA -> SuperState -> Char -> SuperState
deltaSupState nfa s x = Set.map (\(p,a,q) -> q) $ Set.filter (\(p,a,q) -> Set.member p s && Set.member x a) (NFA.rules nfa)

genStatesStep :: NFA -> SuperState -> Set.Set SuperState
genStatesStep nfa ss = Set.map (epsClosureSet nfa) $ Set.map (deltaSupState nfa ss) (NFA.alph nfa)

genStates :: NFA -> Set.Set SuperState -> Set.Set SuperState
genStates nfa ss = Set.foldr Set.union ss $ Set.map (genStatesStep nfa) ss

genStatesAll :: NFA -> Set.Set SuperState -> Set.Set SuperState
genStatesAll nfa ss = if ss == new_ss
                        then ss
                        else genStatesAll nfa new_ss

    where new_ss = Set.union ss $ genStates nfa ss

genRulesStep :: NFA -> SuperState -> Set.Set Rule
genRulesStep nfa ss = Set.fromList $ zipWith (\(p,_,q) s -> (p,s,q)) (map head groupedRules) $ map (Set.fromList . rules2symbols) groupedRules
    
    where newRules      = Set.map (\x -> (ss, x, epsClosureSet nfa $ deltaSupState nfa ss x)) (NFA.alph nfa)
          groupedRules  = List.groupBy (\(_,_,q1) (_,_,q2) -> q1 == q2) $ List.sortBy (\(_,_,q1) (_,_,q2) -> compare q1 q2) $ Set.toList newRules
          rules2symbols = map (\(_,a,_) -> a)

genRules :: NFA -> Set.Set SuperState -> Set.Set Rule
genRules nfa ss = Set.foldr Set.union Set.empty $ Set.map (genRulesStep nfa) ss

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

test_rel = Set.fromList [(1,2),(2,3),(3,4)]

test_nfa_a1 = NFA { NFA.name   = "a"
                  , NFA.states = Set.fromList ["a11", "a12"]
                  , NFA.alph   = Set.singleton 'a'
                  , NFA.rules  = Set.fromList [("a11", Set.singleton 'a', "a12")]
                  , NFA.start  = "a11"
                  , NFA.finish = Set.fromList ["a12"]
                  }

test_nfa_a2 = NFA { NFA.name   = "a"
                  , NFA.states = Set.fromList ["a21", "a22"]
                  , NFA.alph   = Set.singleton 'a'
                  , NFA.rules  = Set.fromList [("a21", Set.singleton 'a', "a22")]
                  , NFA.start  = "a21"
                  , NFA.finish = Set.fromList ["a22"]
                  }                  

test_nfa_b1 = NFA { NFA.name   = "b"
                  , NFA.states = Set.fromList ["b11", "b12"]
                  , NFA.alph   = Set.singleton 'b'
                  , NFA.rules  = Set.fromList [("b11", Set.singleton 'b', "b12")]
                  , NFA.start  = "b11"
                  , NFA.finish = Set.fromList ["b12"]
                  }

test_nfa_b2 = NFA { NFA.name   = "b"
                  , NFA.states = Set.fromList ["b21", "b22"]
                  , NFA.alph   = Set.singleton 'b'
                  , NFA.rules  = Set.fromList [("b21", Set.singleton 'b', "b22")]
                  , NFA.start  = "b21"
                  , NFA.finish = Set.fromList ["b22"]
                  }

test_nfa_b3 = NFA { NFA.name   = "b"
                  , NFA.states = Set.fromList ["b31", "b32"]
                  , NFA.alph   = Set.singleton 'b'
                  , NFA.rules  = Set.fromList [("b31", Set.singleton 'b', "b32")]
                  , NFA.start  = "b31"
                  , NFA.finish = Set.fromList ["b32"]
                  }

test_nfa = NFA.concat (NFA.iter $ NFA.union test_nfa_a1 test_nfa_b1) $ NFA.concat test_nfa_a2 $ NFA.concat test_nfa_b2 test_nfa_b3

