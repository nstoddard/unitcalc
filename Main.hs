import qualified Data.Map as M
import Control.Applicative
import Control.Monad.Trans.Class
import Data.Maybe
import System.Directory
import Control.Exception
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

historyLoc = dataFile "history.txt"
addedUnitsLoc = dataFile "addedUnits.txt"

main = do
    putStrLn $ "unitcalc " ++ showVersion version ++ ", by Nathan Stoddard"
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
                            SExpr {} -> pure ()
                            _ -> lift $ do
                                addedUnitsFilename <- addedUnitsLoc
                                appendFile addedUnitsFilename (input ++ "\n")
                        lift $ putStrLn (prettyPrint res)
                        repl env'

loadFileRepl path env = do
    env' <- lift $ loadFile path env
    case env' of
        Left err -> lift (putStrLn err) >> repl env
        Right env' -> repl env'

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
