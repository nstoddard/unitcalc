{- TODO
Error checking: verify that you can't do something like "unit year/year" with a duplicate string. Also verify that you can't define the same unit more than once
Numbers in the format "1000.0e-1"
Support the syntax "5^-1"
-}

import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.Class
import Data.Maybe
import System.Directory
import Control.Exception
import qualified System.IO.Strict as Strict

import System.Console.Haskeline as H

import Eval
import Types
import Parse

stdlibFilename = "stdlib.txt"
historyFilename = "history.txt"
envFilename = "env.txt"

emptyEnv = Env {envUnits = [], envUnitNames = [],
    envUnitMap = M.empty, envVars = M.empty}

main = do
    exists <- doesFileExist envFilename
    env <- if exists
        then Right . read <$> Strict.readFile envFilename
        else runFile stdlibFilename emptyEnv
    case env of
        Left err -> putStrLn err
        Right env -> do
            env' <- runInputT (Settings noCompletion (Just historyFilename) True) $ repl env
            putStrLn $ "Saving file " ++ envFilename
            writeFile envFilename (show env')

repl :: Env -> InputT IO Env
repl env = do
    input <- replGetInput Nothing
    case input of
        Nothing -> pure env
        Just input -> do
            let stmt = parseInput "" input parseStmt
            case stmt of
                Left err -> lift (putStrLn err) >> repl env
                Right stmt -> case evalStmt stmt env of
                    Left err -> lift (putStrLn err) >> repl env
                    Right (res, env') -> do
                        lift $ putStrLn (prettyPrint res)
                        repl env'


runFile :: String -> Env -> IO (ErrorM Env)
runFile filename env = do
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
