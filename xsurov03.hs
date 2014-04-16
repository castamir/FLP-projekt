import System.IO
import System.Environment
import System.Console.GetOpt
import qualified Data.Set as Set
import qualified Data.List as List
import DFA 
import Interpreter (mfaInterpret,findTrap)
------------------------------------------------------------------------------
data Flag = Invert | PrintMFA
  deriving (Show, Eq)

type State      = String
type SuperState = Set.Set State
type Transition = Set.Set Char
type Rule       = (SuperState, Transition, SuperState)

test_dfa = nfa2dfa test_nfa

test_dfa1 = DFA { DFA.name   = "borek"
               , DFA.states = Set.map Set.fromList $ Set.fromList [["a","b"],["a","c"], ["b", "c"], ["d"], ["f"]]
               , DFA.alph   = Set.fromList ['a','b','c','d']
               , DFA.rules  = Set.fromList [(Set.fromList ["a","b"], Set.fromList ['b','c','d','e','6'], Set.fromList ["f"])]
               , DFA.start  = Set.fromList ["a", "b"]
               , DFA.finish = Set.map Set.fromList $ Set.fromList [["b","c"],["f"]]
               }

------------------------------------------------------------------------------
options :: [OptDescr Flag]
options = 
  [ Option "v" [] (NoArg Invert) "Print non-matching lines"
  , Option "p" [] (NoArg PrintMFA) "Ignore input and print MFA"
  ]

------------------------------------------------------------------------------
-- parse and return parameters (opt, nonOpt) or ioError
getParams :: [String] -> IO ([Flag], [String])
getParams argv =
  case getOpt RequireOrder options argv of
    (opt, [zrv, input], []) -> return (opt, [zrv,input])
    (opt, [zrv], []) -> return (opt, [zrv])
    (_, [], []) -> ioError(userError (emsg1 ++ usage ))
    (_, _, []) -> ioError(userError (emsg2 ++ usage))
    (_ , _, errors) -> ioError(userError (concat errors ++ usage))
  where usage = usageInfo header options ++ zrvInfo ++ inputInfo
        header = "USAGE:\n xsurov03 [-pv] ZRV [INPUT]"
        zrvInfo = "  ZRV\tsimple regular expresssion\n"
        inputInfo = "  INPUT\tfile with input data\n"
        emsg1 = "ZRV is missing!\n"
        emsg2 = "Too many arguments!\n"
------------------------------------------------------------------------------
------------------------------------------------------------------------------
renameMFA_states :: Set.Set SuperState -> SuperState -> [(SuperState, String)]
renameMFA_states states trap = map (renameMFA_states' trap) $ zip (Set.toList states) [0..]
renameMFA_states' trap (x,y)
  | x == trap = (x, "trap")
  | otherwise = (x, "q" ++ show y)
------------------------------------------------------------------------------
findNames :: [SuperState] -> [(SuperState, String)] -> [String]
findNames xs ixs = List.sort [y | z <- xs, (x,y) <- ixs, z == x]

------------------------------------------------------------------------------
names2string :: [String] -> String
names2string xs = (\x -> "{" ++ x ++ "}") $ foldr (++) "" $ List.intersperse "," xs

------------------------------------------------------------------------------
states2string :: [(SuperState, String)] -> String
states2string xs = names2string $ map snd xs

------------------------------------------------------------------------------
rangeStr :: String -> [String]
rangeStr [] = []
rangeStr (c:[]) = [[c]]
rangeStr (c:cs) = rangeStr' [] [c] cs

--------------------------------------
rangeStr' :: [String] -> String -> String -> [String]
rangeStr' xs [] [] = xs
rangeStr' xs ys []
  | length ys > 3 = xs ++ [head ys:'-':last ys:[]]
  | otherwise     = xs ++ [ys]

rangeStr' xs ys (c:cs) =
  if succ (last ys) == c
    then rangeStr' xs (ys ++ [c]) cs
    else if length ys > 2
      then rangeStr' (xs ++ [head ys:'-':last ys:[]]) [c] cs
      else rangeStr' (xs ++ [ys]) [c] cs

------------------------------------------------------------------------------
padding :: String -> Int -> String
padding xs i  
  | length xs >= i  = xs
  | otherwise = xs ++ [s | s <- " ", y <- [0..(i - length xs)]]

------------------------------------------------------------------------------
printRules :: [(SuperState, String)] -> [Rule] -> IO ()
printRules states ((src, symbols, dst):rs) = do
  putStr $ "  " ++ padding (head $ findNames [src] states) 5
  putStr $ "| " ++ padding (head $ findNames [dst] states) 5
  putStrLn  $ "| " ++ strSymb
  if length rs == 0
    then return ()
    else printRules states rs
  where
      strSymb = names2string $ rangeStr $ Set.toList symbols

------------------------------------------------------------------------------
printMFA :: DFA -> IO ()
printMFA mfa = do 
  putStr "States:        "
  putStrLn $ states2string istates
  putStr "Alphabet:      "
  putStrLn $ names2string $ List.intersperse "," $ rangeStr $ Set.toList $ DFA.alph mfa
  putStr "Start state:   "
  putStrLn $ head $ findNames [DFA.start mfa] istates
  putStr "Final states:  "
  putStrLn $ names2string $ findNames ( Set.toList (DFA.finish mfa)) istates
  putStrLn "Rules:"
  putStrLn " Source | Dest. | Symbols"
  putStrLn "------------------------------"
  printRules istates $ Set.toList $ DFA.rules mfa 
  putStrLn "------------------------------"
  where
    trap = findTrap (Set.toList $ DFA.rules mfa) (DFA.finish mfa) (DFA.alph mfa)
    istates = renameMFA_states (DFA.states mfa) trap

------------------------------------------------------------------------------
matchLine :: String -> DFA -> Bool
matchLine cs mfa 
  | cs == []  = mfaInterpret cs (DFA.start mfa) mfa
  | mfaInterpret cs (DFA.start mfa) mfa == False = matchLine (tail cs) mfa
  | otherwise = True

------------------------------------------------------------------------------
executeSimpleGrep :: DFA -> Handle -> IO ()
executeSimpleGrep mfa handle = do
  -- get line and match regexp by MFA
  ieof <- hIsEOF handle
  if ieof
    then return ()
    else do
      line <- hGetLine handle
      if matchLine line mfa == False
        then return ()
        else putStrLn line
      executeSimpleGrep mfa handle
------------------------------------------------------------------------------
main = do
    -- get parameters
    argv <- getArgs
    (opt, nonOpt) <- getParams argv
    -- call praser
    -- FSM ... MFA
    -- check if option "-p" be setted
    if elem PrintMFA opt
      then printMFA test_dfa
      else do
        if length nonOpt == 2
          then do -- input data from file
            handle <- openFile (last nonOpt) ReadMode
            executeSimpleGrep test_dfa handle 
            hClose handle
          else -- input data from stdin
            executeSimpleGrep test_dfa stdin

    return 0

