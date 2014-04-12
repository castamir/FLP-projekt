-- sestavit FA a od nej derivovat ostatni automaty typove je to totez
module FA
(FA(..)
)where

import qualified Data.Set as Set

type Symbol = Set.Set Char
type State  = String
type Rule   = (State, Symbol, State)

data FA = FA { name   :: String
              , states :: Set.Set State
              , alph   :: Set.Set Symbol
              , rules  :: Set.Set Rule
              , start  :: State
              , finish :: Set.Set State
              } deriving (Show)


-- eliminace nedosazitelnych stavu??


