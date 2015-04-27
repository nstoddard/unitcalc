module Util where

import Control.Applicative
import Data.Char
import System.IO

flush :: IO ()
flush = hFlush stdout

prompt :: String -> IO String
prompt msg = do
  putStr msg >> flush
  getLine

prompt' :: [(Char,b)] -> String -> IO b
prompt' xs msg = do
  response <- prompt msg
  if null response
    then prompt' xs msg
    else maybe (prompt' xs msg) pure $
      lookup (toLower $ head response) xs

yesno = prompt' [('y',True),('n',False)]
