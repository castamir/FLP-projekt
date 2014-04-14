import System.IO
import System.Environment
import System.Console.GetOpt
import qualified Data.Set as Set
import qualified Data.List as List
import DFA 
------------------------------------------------------------------------------
data Flag = Invert | PrintMFA
  deriving (Show, Eq)

type State      = String
type SuperState = Set.Set State
type Transition = Set.Set Char
type Rule       = (SuperState, Transition, SuperState)

test_dfa = DFA { DFA.name   = "borek"
               , DFA.states = Set.map Set.fromList $ Set.fromList [["a","b"],["a","c"], ["b", "c"], ["d"], ["f"]]
               , DFA.alph   = Set.fromList ['a','b','c','d']
               , DFA.rules  = Set.fromList [(Set.fromList ["a","b"], Set.singleton 'b', Set.fromList ["f"])]
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
renameMFA_states :: Set.Set SuperState -> [(SuperState, String)]
renameMFA_states states = map renameMFA_states' $ zip (Set.toList states) [0..]
renameMFA_states' (x,y) = (x, "q" ++ show y)
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
rangeStr :: [String] -> [Char] -> [String]
rangeStr xs [] = xs
rangeStr xs (c:[]) = [[c]]
rangeStr xs (c:cs) = rangeStr' xs c cs

rangeStr' :: [String] -> Char -> [Char] -> [String]
rangeStr' xs p [] = xs ++ [[p]]
rangeStr' xs p (c1:[])
rangeStr' xs p (c1:c2:[]) =
  if succ p == c1
    then rangeStr' xs p (c2:cs)
    else rangeStr' (xs ++ [p:])
-- rangeStr' xs p (c:[]) = 
-- rangeStr' xs (c:d:cs) = xs ++ [[c]]

------------------------------------------------------------------------------
printMFA :: DFA -> IO ()
printMFA mfa = do 
  let istates = renameMFA_states $ DFA.states mfa
  putStr "States:        "
  putStrLn $ states2string istates
  putStr "Alphabet:      "
  putStrLn $ names2string $ [List.intersperse ',' $ Set.toList $ DFA.alph mfa]
  putStr "Start state:   "
  putStrLn $ head $ findNames [DFA.start mfa] istates
  putStr "Finish states: "
  putStrLn $ names2string $ findNames ( Set.toList (DFA.finish mfa)) istates
  putStrLn "Rules:"
  putStrLn " Source | Dest.| Symbols"
  putStrLn "------------------------------"
  putStrLn "  q011  | q123 | hlakjdh"
  putStrLn ""
  --print_states $ DFA.states mfa
  --print_alph $ DFA.alph mfa
  --print_rules
  --print_states $ [DFA.start mfa]

------------------------------------------------------------------------------
executeSimpleGrep :: String -> Handle -> IO ()
executeSimpleGrep _mfa handle = do
  -- nacti radek zavolej funkci
  ieof <- hIsEOF handle
  if ieof
    then return ()
    else do
      line <- hGetLine handle
      putStrLn line
      executeSimpleGrep _mfa handle
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
            executeSimpleGrep "lala" handle 
            hClose handle
          else -- input data from stdin
            executeSimpleGrep "lala" stdin

    return 0

