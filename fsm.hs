import qualified Data.Set as Set

type Symbol = Maybe Char
type State  = String
type Rule   = (State, Symbol, State)

data FSM = FSM  { name   :: String
                , states :: Set.Set State
                , alph   :: Set.Set Symbol
                , rules  :: Set.Set Rule
                , start  :: State
                , finish :: Set.Set State
                } deriving (Show)

isDisjoint :: Ord a => Set.Set a -> Set.Set a -> Bool
isDisjoint s1 s2 = Set.null $ Set.intersection s1 s2

fsmConcat :: FSM -> FSM -> FSM
fsmConcat m1 m2 = if isDisjoint (states m1) (states m2)
                    then FSM { name   = newName
                             , states = Set.union (states m1) (states m2)
                             , alph   = Set.union (alph m1) (alph m2)
                             , rules  = Set.union bridge $ Set.union (rules m1) (rules m2)
                             , start  = start m1
                             , finish = finish m2 
                             }
                    else error "FSM.concat: State sets are not disjoint."

    where newName = name m1 ++ name m2
          bridge  = Set.fromList [(p,a,q) | p <- Set.toList (finish m1), a <- [Nothing], q <- [start m2]]

fsmUnion :: FSM -> FSM -> FSM
fsmUnion m1 m2 = if isDisjoint (states m1) (states m2)
                    then FSM { name   = newName
                             , states = Set.union (Set.singleton newStart) $ Set.union newFinish $ Set.union (states m1) (states m2)
                             , alph   = Set.union (alph m1) (alph m2)
                             , rules  = Set.union fork $ Set.union join $ Set.union (rules m1) (rules m2)
                             , start  = newStart
                             , finish = newFinish 
                             }
                    else error "FSM.union: State sets are not disjoint."

    where newName   = name m1 ++ "+" ++ name m2
          newStart  = "S_" ++ newName
          newFinish = Set.singleton ("F_" ++ newName)
          fork      = Set.fromList [(newStart, Nothing, start m1), (newStart, Nothing, start m2)]
          join      = Set.fromList [(p,a,q) | p <- Set.toList $ Set.union (finish m1) (finish m2), a <- [Nothing], q <- Set.toList newFinish]

--fsmIteration :: FSM -> FSM -> FSM
--fsmIteration m1 = 

fsm_a = FSM { name   = "a"
            , states = Set.fromList ["a1", "a2"]
            , alph   = Set.fromList [Just 'a']
            , rules  = Set.fromList [("a1", Just 'a', "a2")]
            , start  = "a1"
            , finish = Set.fromList ["a2"]
            }

fsm_b = FSM { name   = "b"
            , states = Set.fromList ["b1", "b2"]
            , alph   = Set.fromList [Just 'b']
            , rules  = Set.fromList [("b1", Just 'b', "b2")]
            , start  = "b1"
            , finish = Set.fromList ["b2"]
            }
