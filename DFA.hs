module DFA
( DFA(..)
, nfa2dfa
)where

import qualified Data.Set as Set
import NFA

type Symbol = Set.Set Char
type State  = String
type Rule   = (State, Symbol, State)

data DFA = DFA { name   :: String
               , states :: Set.Set State
               , alph   :: Set.Set Symbol
               , rules  :: Set.Set Rule
               , start  :: State
               , finish :: Set.Set State
               } deriving (Show)

reflxClosure :: Ord a => Set.Set (a,a) -> Set.Set (a,a)
reflxClosure s = Set.union s $ Set.fromList [(fst a, fst a) | a <- Set.toList s]

transClosure :: Ord a => Set.Set (a,a) -> Set.Set (a,a)
transClosure s = if s == trans
                  then s
                  else transClosure trans

    where trans = Set.union s $ Set.fromList [(fst a, snd b) | a <- Set.toList s, b <- Set.toList s, snd a == fst b]

--nfa2dfa :: NFA -> DFA
nfa2dfa nfa = nfa


--epsTransition   = Set.fromList [(p,q) | (p,a,q) <- Set.toList (rules nfa), a == Nothing]
--epsClosureAll   = transClosure $ reflxClosure epsTransition
--epsClosure p    = Set.filter (\x -> fst x == p) epsClosureAll
--epsClosureSet s = Set.unions $ Set.map epsClosure s

test_rel = Set.fromList [(1,2),(2,3),(3,4)]
