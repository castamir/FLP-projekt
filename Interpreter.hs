------------------------------------------------------
---- Project: Simple Grep
---- Authors:
----   xsurov03 - Marek Surovic
----   xstodu05 - Petr Stodulka
----   xpavlu06 - Igor Pavlu
----   xpauli00 - Miroslav Paulik
--------------------------------------------------------

module Interpreter
( mfaInterpret
, findTrap
) where

import qualified Data.Set as Set
import DFA

type State      = String
type SuperState = Set.Set State
type Transition = Set.Set Char
type Rule       = (SuperState, Transition, SuperState)

------------------------------------------------------------------------------
-- find trap - if exists return it else return unique state "_"
findTrap :: [Rule] -> Set.Set SuperState -> Set.Set Char -> SuperState
findTrap [] _ _ = Set.singleton ['_'] -- trap doesn't exists, so return special Superstate
findTrap ((p, r, q):rs) fs cs
  | r == cs && p == q && Set.member p fs == False  = p
  | otherwise  = findTrap rs fs cs

-----------------------------------------------------------------------------
-- find rule for state and symbol and return next state
findNextState :: SuperState->  Char -> [Rule] -> SuperState
findNextState state symbol [] = error "findNextState: not found next state"
findNextState state symbol ((p, a, q):rs)
  | state == p && Set.member symbol a == True = q
  | otherwise = findNextState state symbol rs

------------------------------------------------------------------------------
-- interpret of DFA (or MFA)
-- return True if reach final state or False if don't reach final state or
-- reach "trap" state
mfaInterpret :: String -> SuperState -> DFA -> Bool
mfaInterpret [] state mfa = Set.member state $ DFA.finish mfa
mfaInterpret (c:cs) state mfa
  | Set.member state fins == True  = True
  | state == trap                  = False
  | Set.member c alph == False = False
  | otherwise = mfaInterpret cs nextState mfa
  where
    fins = DFA.finish mfa
    rules = Set.toList $ DFA.rules mfa
    nextState = findNextState state c rules
    alph = DFA.alph mfa
    trap = findTrap rules fins alph

