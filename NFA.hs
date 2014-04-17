module NFA
( NFA(..)
, ast2nfa
)where

import Prelude hiding (concat)

import qualified Data.Set  as Set
import qualified Data.List as List

import Parser

type State      = String
type Transition = Set.Set Char
type Rule       = (State, Transition, State)

data NFA = NFA { name   :: String
               , states :: Set.Set State
               , alph   :: Set.Set Char
               , rules  :: Set.Set Rule
               , start  :: State
               , finish :: Set.Set State
               } deriving (Show)              

renameStates :: Set.Set State -> Set.Set State
renameStates s = Set.map (++ "'") s

concat :: NFA -> NFA -> NFA
concat m1 m2 = NFA { name   = newName
                   , states = Set.union (renameStates $ states m1) (states m2)
                   , alph   = Set.union (alph m1) (alph m2)
                   , rules  = Set.union bridge $ Set.union (rules m1) (rules m2)
                   , start  = start m1
                   , finish = finish m2 
                   }

    where newName = name m1 ++ name m2
          bridge  = Set.fromList [(p,a,q) | p <- Set.toList (finish m1), a <- [Set.empty], q <- [start m2]]

union :: NFA -> NFA -> NFA
union m1 m2 = NFA { name   = newName
                  , states = Set.union (Set.singleton newStart) $ Set.union (Set.singleton newFinish) $ Set.union (renameStates $ states m1) (states m2)
                  , alph   = Set.union (alph m1) (alph m2)
                  , rules  = Set.union fork $ Set.union join $ Set.union (rules m1) (rules m2)
                  , start  = newStart
                  , finish = Set.singleton newFinish 
                  }

    where newName   = name m1 ++ "+" ++ name m2
          newStart  = "S_" ++ newName
          newFinish = "F_" ++ newName
          fork      = Set.fromList [(newStart, Set.empty, start m1), (newStart, Set.empty, start m2)]
          join      = Set.fromList [(p,a,q) | p <- Set.toList $ Set.union (finish m1) (finish m2), a <- [Set.empty], q <- [newFinish]]

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
          fromStart = Set.singleton (newStart, Set.empty, start m)
          toFinish  = Set.fromList [(p,a,q) | p <- Set.toList (finish m), a <- [Set.empty], q <- [newFinish]]
          bypass    = Set.singleton (newStart, Set.empty, newFinish)
          loop      = Set.fromList [(p,a,q) | p <- Set.toList (finish m), a <- [Set.empty], q <- [start m]]

ast2nfa :: BTree -> NFA
ast2nfa (Branch left op right)
    | op == '.' = concat (ast2nfa left) (ast2nfa right)
    | op == '|' = union (ast2nfa left) (ast2nfa right)
    | op == '*' = iter (ast2nfa left)

ast2nfa (Leaf x) = NFA { name   = Set.toList x
                       , states = Set.union (Set.singleton newFinish) (Set.singleton newStart)
                       , alph   = x
                       , rules  = Set.singleton (newStart, x, newFinish)
                       , start  = newStart
                       , finish = Set.singleton newFinish
                       }

    where newStart  = "S_{" ++ (List.intersperse ',' $ Set.toList x) ++ "}"
          newFinish = "F_{" ++ (List.intersperse ',' $ Set.toList x) ++ "}"
