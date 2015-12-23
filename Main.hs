{- TODO
High priority:
    Support numbers in the format "1000.0e-1"
    Ban things like "10 m -> 10 m" which doesn't make any sense.
    Allow omitting the leading 0 in ".5"
    Add rounding to output so you don't see stuff like this:
        > parsec -> lightYear
        3.2599999999999993 lightYear

Error checking: verify that you can't do something like "unit year/year" with a duplicate string.
    Also verify that you can't define the same unit more than once
Support the syntax "5^-1".
Don't automatically convert to base units; keep the original units where possible.
    E.g. "5 ft * 10 ft" should give an answer in ft^2.
Converting to a sum of multiple units (e.g. feet + inches)
Treat "0" the same as "0 meters" etc
Clean up the code
Command-line options?
Add a 'help' command
Release executables
Put on Hackage?
Functions
Allow commas in numbers
Put each output in a varible.
New unit definitions should override previous ones
Use my 'repl' lib (currently unreleased)

Bugs:
    Sometimes something goes wrong when saving the environment, and it ends up with zero bytes. This happens very rarely and I can't reproduce it.
    Sometimes pressing ctrl+c results in it saving the environment twice. This shouldn't be possible.
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
import Control.Monad

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
addedUnitsLoc = dataFile "addedUnits.txt"

emptyEnv = Env {envUnits = [], envUnitNames = [],
    envUnitMap = M.empty, envVars = M.empty}

-- TODO: do I need to run setUnicode?
main = do
    putStrLn $ "unitcalc " ++ showVersion version ++ ", by Nathan Stoddard"
    -- TODO: why not run this when saving state, instead of here?
    createDirectoryIfMissing True =<< dataDir
    stdlibFilename <- stdlibLoc
    addedUnitsFilename <- addedUnitsLoc
    historyFilename <- historyLoc

    addedUnitsExists <- doesFileExist addedUnitsFilename
    env <- loadFile stdlibFilename emptyEnv
    env <- if addedUnitsExists then join <$> mapM (loadFile addedUnitsFilename) env else pure env

    case env of
        Left err -> putStrLn err
        Right env -> void $ runInputT (Settings noCompletion (Just historyFilename) True) $ repl env


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
                Right (RStmt stmt) -> case evalStmt stmt env of
                    Left err -> lift (putStrLn err) >> repl env
                    Right (res, env') -> do
                        case stmt of
                            SUnitDef {} -> lift $ do
                                addedUnitsFilename <- addedUnitsLoc
                                appendFile addedUnitsFilename (input ++ "\n")
                            _ -> pure ()
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
