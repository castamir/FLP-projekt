import System.IO
import System.Environment
import System.Console.GetOpt
import Data.Maybe (fromMaybe)

------------------------------------------------------------------------------
data Flag = Invert | PrintMFA
  deriving (Show, Eq)

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
printMFA = do
  return ()


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
    print opt
    print nonOpt
    if elem PrintMFA opt
      then printMFA
      else do
        if length nonOpt == 2
          then do -- input data from file
            handle <- openFile (last nonOpt) ReadMode
            executeSimpleGrep "lala" handle 
            hClose handle
          else -- input data from stdin
            executeSimpleGrep "lala" stdin

    return 0

-- getFileLines :: String -> [String]
fileBranch file = do
    handle <-openFile file ReadMode
    contents <- hGetContents handle
    hClose handle
    return 0

