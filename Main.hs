{- TODO
Error checking: verify that you can't do something like "unit year/year" with a duplicate string.
    Also verify that you can't define the same unit more than once
Support numbers in the format "1000.0e-1"
Support the syntax "5^-1"
Add rounding to output so you don't see stuff like this:
    > parsec -> lightYear
    3.2599999999999993 lightYear
Make it possible to reload the stdlib without deleting env.txt
Don't automatically convert to base units; keep the original units where possible.
    E.g. "5 ft * 10 ft" should give an answer in ft^2.
Find a way to reduce the size of env.txt. The SI prefixes really increase its size.
Bits, bytes, kibi- and other prefixes
Allow omitting the leading 0 in ".5"
Converting to a sum of multiple units (e.g. feet + inches)
Use Cabal
Treat "0" the same as "0 meters" etc
Ban things like "10 m -> 10 m" which doesn't make any sense.
Clean up the code
Command-line options?
Add a 'help' command
-}

import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.Class
import Data.Maybe
import System.Directory
import Control.Exception
import qualified System.IO.Strict as Strict
import qualified System.FilePath as FP
import Data.Version

import qualified Text.PrettyPrint.Leijen as P
import Text.PrettyPrint.Leijen ((<//>), (</>), Pretty, pretty, displayS, renderPretty)

import System.Console.Haskeline as H

import Paths_unitcalc

import Eval
import Types
import Parse
import Util

dataDir = getAppUserDataDirectory "unitcalc"
dataFile filename = (FP.</> filename) <$> dataDir

stdlibLoc = getDataFileName "stdlib.txt"
historyLoc = dataFile "history.txt"
envLoc = dataFile "env.txt"

emptyEnv = Env {envUnits = [], envUnitNames = [],
    envUnitMap = M.empty, envVars = M.empty}

main = do
    putStrLn $ "unitcalc " ++ showVersion version ++ ", by Nathan Stoddard"
    createDirectoryIfMissing False =<< dataDir
    stdlibFilename <- stdlibLoc
    historyFilename <- historyLoc
    envFilename <- envLoc
    exists <- doesFileExist envFilename
    env <- if exists
        then Right . read <$> Strict.readFile envFilename
        else loadFile stdlibFilename emptyEnv
    case env of
        Left err -> putStrLn err
        Right env -> do
            env' <- runInputT (Settings noCompletion (Just historyFilename) True) $ repl env
            putStrLn $ "Saving file " ++ envFilename
            writeFile envFilename (show env')


-- Pretty-prints a string by splitting it into words so they can be properly 
-- split across lines
prettyString = P.fillSep . map pretty . words

repl :: Env -> InputT IO Env
repl env = do
    input <- replGetInput Nothing
    case input of
        Nothing -> pure env
        Just input -> do
            let stmt = parseInput "" input parseReplCmd
            case stmt of
                Left err -> lift (putStrLn err) >> repl env
                Right (RLoad path) -> loadFileRepl path env
                Right RReset -> do
                    doIt <- lift $ yesno $ prettyPrint $ prettyString "Really reset the environment? This will delete every unit and variable except those in the standard library." P.</> pretty "[enter Y or N]  > "
                    if doIt
                        then do
                            stdlibFilename <- lift stdlibLoc
                            loadFileRepl stdlibFilename emptyEnv
                        else repl env
                Right (RStmt stmt) -> case evalStmt stmt env of
                    Left err -> lift (putStrLn err) >> repl env
                    Right (res, env') -> do
                        lift $ putStrLn (prettyPrint res)
                        repl env'

loadFileRepl path env = do
    env' <- lift $ loadFile path env
    case env' of
        Left err -> lift (putStrLn err) >> repl env
        Right env' -> repl env'

loadFile :: String -> Env -> IO (ErrorM Env)
loadFile filename env = do
    exists <- doesFileExist filename
    if not exists then pure (Left $ "File doesn't exist: " ++ filename) else do
    input <- Strict.readFile filename
    let stmts = parseInput filename input parseStmts
    case stmts of
        Left err -> pure (Left err)
        Right stmts -> do
            putStrLn $ "Running file: " ++ filename
            pure $ evalStmts stmts env

handleCtrlC = H.handle . ctrlC where
    ctrlC :: a -> AsyncException -> InputT IO a
    ctrlC def UserInterrupt = pure def
    ctrlC def e = lift (putStrLn $ "Unknown exception: " ++ show e) >> pure def

replGetInput cont = do
    let prompt = if isJust cont then "... " else "> "
    input_ <- handleCtrlC (Just "") $ getInputLine prompt
    if isNothing input_ then pure Nothing else do
    let input = fromJust input_
    if null input || input == "exit" then pure Nothing else do
    let
        input' = case cont of
            Just cont -> cont ++ "\n" ++ input
            Nothing -> input
    if countParens input' > 0 then replGetInput (Just input')
        else pure (Just input')

countParens [] = 0
countParens (x:xs)
    | x `elem` "(" = countParens xs + 1
    | x `elem` ")" = countParens xs - 1
    | otherwise = countParens xs
