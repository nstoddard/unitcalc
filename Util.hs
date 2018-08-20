module Util where

import qualified System.IO.Strict as Strict
import System.Directory

import Types
import Eval
import Parse

import Paths_unitcalc

stdlibLoc = getDataFileName "stdlib.txt"

loadFile :: String -> Env -> IO (ErrorM Env)
loadFile filename env = do
    exists <- doesFileExist filename
    if not exists then pure (Left $ "File doesn't exist: " ++ filename) else do
    input <- Strict.readFile filename
    let stmts = parseInput filename input parseStmts
    pure $ case stmts of
        Left err -> Left err
        Right stmts -> evalStmts stmts env
