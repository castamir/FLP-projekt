------------------------------------------------------
-- Project: Simple Grep
-- Authors:
--   xsurov03 - Marek Surovic
--   xstodu05 - Petr Stodulka
--   xpavlu06 - Igor Pavlu
--   xpauli00 - Miroslav Paulik
------------------------------------------------------

import System.IO
import System.Environment
import System.Console.GetOpt
import qualified Data.Set as Set
import qualified Data.List as List

import Parser
import NFA
import DFA 
import Interpreter (mfaInterpret,findTrap)
------------------------------------------------------------------------------
data Flag = Invert | PrintMFA
  deriving (Show, Eq)

type State      = String
type SuperState = Set.Set State
type Transition = Set.Set Char
type Rule       = (SuperState, Transition, SuperState)

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
-- transform states into readable format
-- if state if "trap" - call it "trap"
renameMFA_states :: Set.Set SuperState -> SuperState -> [(SuperState, String)]
renameMFA_states states trap = map (renameMFA_states' trap) $ zip (Set.toList states) [0..]

-- (i know that's little ugly split of functions)
-- choose label "trap" or qi where i <- [0,1..]
renameMFA_states' trap (x,y)
  | x == trap = (x, "trap")
  | otherwise = (x, "q" ++ show y)

------------------------------------------------------------------------------
-- find/return readable assigned names of states in xs from ixs
-- xs is subset of ixs
findNames :: [SuperState] -> [(SuperState, String)] -> [String]
findNames xs ixs = List.sort [y | z <- xs, (x,y) <- ixs, z == x]

------------------------------------------------------------------------------
-- transform names (or just list of strings) into one string with commas between
-- origin strings and curly brackets on the sides
names2string :: [String] -> String
names2string xs = (\x -> "{" ++ x ++ "}") $ foldr (++) "" $ List.intersperse "," xs

------------------------------------------------------------------------------
-- helper function for print all states 
states2string :: [(SuperState, String)] -> String
states2string xs = names2string $ map snd xs

------------------------------------------------------------------------------
-- transform  succession  into shorter format (order longer then 3 chars)
-- e.g.: abcdefg -> a-g
--  abc -> abc
--  (this master only check input conditions for slave)
rangeStr :: String -> [String]
rangeStr [] = []
rangeStr (c:[]) = [[c]]
rangeStr (c:cs) = rangeStr' [] [c] cs

--------------------------------------
-- slave of function above
rangeStr' :: [String] -> String -> String -> [String]
rangeStr' xs [] [] = xs
rangeStr' xs ys [] --no more chars
  | length ys > 3 = xs ++ [head ys:'-':last ys:[]]
  | otherwise     = xs ++ [List.intersperse ',' ys]

rangeStr' xs ys (c:cs) =
  if succ (last ys) == c
    then rangeStr' xs (ys ++ [c]) cs -- add char into tmp string
    else if length ys > 2 -- c is not next successor of order - string completed
      then rangeStr' (xs ++ [head ys:'-':last ys:[]]) [c] cs
      else rangeStr' (xs ++ [List.intersperse ',' ys]) [c] cs

------------------------------------------------------------------------------
-- add spaces after string to specified length
padding :: String -> Int -> String
padding xs i  
  | length xs >= i  = xs
  | otherwise = xs ++ [s | s <- " ", y <- [0..(i - length xs)]]

------------------------------------------------------------------------------
-- pretty print of all rules - one rule on line
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
-- complete print of whole DFA (or MFA in this case)
printMFA :: DFA -> IO ()
printMFA mfa = do 
  putStr "States:        "
  putStrLn $ states2string istates
  putStr "Alphabet:      "
  putStrLn $ names2string $ rangeStr $ Set.toList $ DFA.alph mfa
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
-- if line corresponds to regexp represented by DFA (or MFA) return true
matchLine :: String -> DFA -> Bool
matchLine cs mfa 
  | cs == []  = mfaInterpret cs (DFA.start mfa) mfa
  | mfaInterpret cs (DFA.start mfa) mfa == False = matchLine (tail cs) mfa
  | otherwise = True

------------------------------------------------------------------------------
-- own process of our simple grep
executeSimpleGrep :: DFA -> Handle -> Bool -> IO ()
executeSimpleGrep mfa handle _bool = do
  -- get line and match regexp by MFA
  ieof <- hIsEOF handle
  if ieof
    then return ()
    else do
      line <- hGetLine handle
      -- _bool = False if is used "-v" parameter, else True
      if (matchLine line mfa) == _bool
        then putStrLn line
        else return ()
      executeSimpleGrep mfa handle _bool
------------------------------------------------------------------------------
main = do
    -- get parameters
    argv <- getArgs
    (opt, nonOpt) <- getParams argv
    -- parse regexp -> create tree -> create NFA -> transform to DFA
    let mfa = DFA.nfa2dfa $ fst $ NFA.ast2nfa ((Parser.regexpToBTree $ head nonOpt), 0)
    if elem PrintMFA opt
      then printMFA mfa
      else do
        if length nonOpt == 2
          then do -- input data from file
            handle <- openFile (last nonOpt) ReadMode
            -- when "-v" is used, length opt == 1 - so genereate False
            -- for negated output
            executeSimpleGrep mfa handle $ length opt == 0
            hClose handle
          else -- input data from stdin
            executeSimpleGrep mfa stdin $ length opt == 0
    return 0
