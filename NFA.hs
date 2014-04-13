module NFA
( NFA(..)
, concat
, union
, iter
)where

import Prelude hiding (concat)
import qualified Data.Set as Set

type State      = String
type Transition = Maybe (Set.Set Char)
type Rule       = (State, Transition, State)

data NFA = NFA { name   :: String
               , states :: Set.Set State
               , alph   :: Set.Set Char
               , rules  :: Set.Set Rule
               , start  :: State
               , finish :: Set.Set State
               } deriving (Show)

isDisjoint :: Ord a => Set.Set a -> Set.Set a -> Bool
isDisjoint s1 s2 = Set.null $ Set.intersection s1 s2

concat :: NFA -> NFA -> NFA
concat m1 m2 = if isDisjoint (states m1) (states m2)
                    then NFA { name   = newName
                             , states = Set.union (states m1) (states m2)
                             , alph   = Set.union (alph m1) (alph m2)
                             , rules  = Set.union bridge $ Set.union (rules m1) (rules m2)
                             , start  = start m1
                             , finish = finish m2 
                             }
                    else error "NFA.concat: State sets are not disjoint."

    where newName = name m1 ++ name m2
          bridge  = Set.fromList [(p,a,q) | p <- Set.toList (finish m1), a <- [Nothing], q <- [start m2]]

union :: NFA -> NFA -> NFA
union m1 m2 = if isDisjoint (states m1) (states m2)
                    then NFA { name   = newName
                             , states = Set.union (Set.singleton newStart) $ Set.union (Set.singleton newFinish) $ Set.union (states m1) (states m2)
                             , alph   = Set.union (alph m1) (alph m2)
                             , rules  = Set.union fork $ Set.union join $ Set.union (rules m1) (rules m2)
                             , start  = newStart
                             , finish = Set.singleton newFinish 
                             }
                    else error "NFA.union: State sets are not disjoint."

    where newName   = name m1 ++ "+" ++ name m2
          newStart  = "S_" ++ newName
          newFinish = "F_" ++ newName
          fork      = Set.fromList [(newStart, Nothing, start m1), (newStart, Nothing, start m2)]
          join      = Set.fromList [(p,a,q) | p <- Set.toList $ Set.union (finish m1) (finish m2), a <- [Nothing], q <- [newFinish]]

iter :: NFA -> NFA
iter m = NFA { name   = newName
             , states = Set.union (Set.singleton newStart) $ Set.union (Set.singleton newFinish) (states m)
             , alph   = alph m
             , rules  = Set.union bypass $ Set.union loop $ Set.union toFinish $ Set.union fromStart (rules m)
             , start  = newStart
             , finish = Set.singleton newFinish 
             }

    where newName   = if length (name m) > 1 then "(" ++ name m ++ ")*" else name m ++ "*"
          newStart  = "S_" ++ newName
          newFinish = "F_" ++ newName
          fromStart = Set.singleton (newStart, Nothing, start m)
          toFinish  = Set.fromList [(p,a,q) | p <- Set.toList (finish m), a <- [Nothing], q <- [newFinish]]
          bypass    = Set.singleton (newStart, Nothing, newFinish)
          loop      = Set.fromList [(p,a,q) | p <- Set.toList (finish m), a <- [Nothing], q <- [start m]]

test_nfa_a = NFA { name   = "a"
                 , states = Set.fromList ["a1", "a2"]
                 , alph   = Set.singleton 'a'
                 , rules  = Set.fromList [("a1", Just (Set.singleton 'a'), "a2")]
                 , start  = "a1"
                 , finish = Set.fromList ["a2"]
                 }

test_nfa_b = NFA { name   = "b"
                 , states = Set.fromList ["b1", "b2"]
                 , alph   = Set.singleton 'b'
                 , rules  = Set.fromList [("b1", Just (Set.singleton 'b'), "b2")]
                 , start  = "b1"
                 , finish = Set.fromList ["b2"]
                 }
