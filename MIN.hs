module MIN
(FA(..)
,MIN(..)
,CLASS(..)
, makeClass
, makeMinimalize
) where
--, min2fa

import qualified Data.Set as Set
-- tento balast se da nekam bokem do automatu FA tady je jen protoze mi to rvalo a nechcelo se mi s tim resit
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
-- konec balastu 

--import FA

data CLASS = CLASS { classId :: Int
					, stateField :: Set.Set State
			 		} deriving (Show, Eq, Ord)

data MIN = MIN { classC :: Set.Set CLASS
				, rulesC :: Set.Set Rule
				, startC :: State
				, finishC :: Set.Set State
				} deriving (Show, Eq)

-- prevede konecny automat FA na strukturu MIN
makeClass :: FA -> MIN
makeClass fa = 	MIN { classC = Set.union (Set.singleton (clsfin)) (Set.singleton (clsrest))
					, rulesC = rules fa
					, startC = start fa
					, finishC = finish fa
					} where
						clsfin = CLASS{
							classId = 0
							,stateField = finish fa
						}
						clsrest = CLASS{
							classId = 1
							, stateField = Set.difference (finish fa) (states fa) 
						}	

-- vezme strukturu MIN a prevede na Minimalizovanejsi verzi poku jsou 2 verze stejne tak je to vysledek				 
makeMinimalize :: MIN -> MIN
makeMinimalize m = m	

-- prevod minimalizovane verze na minimalni automat
--min2fa :: MIN -> FA
--min2fa m = fa		 

-- provede celou minimalizaci z determinizovaneho automatu FA na minimalni automat f2
runMin :: FA -> FA
runMin fa = fa2 where
			m = makeClass fa
			m2 = makeMinimalize m
			--fa2 = min2fa m2
			fa2 = fa