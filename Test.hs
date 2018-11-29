import Test.HUnit
import System.IO.Unsafe
import Control.Monad
import Test.Framework
import Test.Framework.Providers.HUnit

import Eval
import Parse
import Types
import Util

parseExprForTest input = parseInput "" input parseExpr

evalExprForTest :: String -> IO (ErrorM Value)
evalExprForTest str = do
  stdlibFilename <- stdlibLoc
  let expr = parseExprForTest str
  env <- loadFile stdlibFilename emptyEnv
  case expr of
    Right _ -> pure ()
    Left _ -> assertFailure "Invalid expr"
  case env of
    Right _ -> pure ()
    Left _ -> assertFailure "Invalid env"
  -- This part takes place in the ErrorM monad, so we need to also wrap it in IO
  pure $ join $ evalExpr <$> expr <*> env

assertEqualExprs :: String -> String -> IO ()
assertEqualExprs a b = do
  a' <- evalExprForTest a
  b' <- evalExprForTest b
  assertEqual "" a' b'

mathTests = TestList
  [ TestCase (assertEqualExprs "2+2" "4")
  , TestCase (assertEqualExprs "2+3 * 4" "20")
  , TestCase (assertEqualExprs "5^-1" "5 ^ -1")
  , TestCase (assertEqualExprs "5^-1 m" "5^(-1) m")
  ]

conversionTests = TestList
  [ TestCase (assertEqualExprs "2 m @ cm" "200 cm")
  , TestCase (assertEqualExprs "2 m^2 @ cm^2" "20000 cm^2")
  , TestCase (assertEqualExprs "((1 ft) @ inches) + (1 inch)" "(1 ft) @ inches + (1 inch)")
  ]

functions = TestList
  [ TestCase (assertEqualExprs "(\\x,y->x+y) 3 5" "8")
  , TestCase (assertEqualExprs "((\\x -> (\\y -> x + y)) 4) 5" "9")
  ]

tests = TestList [mathTests, conversionTests, functions]

main = defaultMain $ hUnitTestToTests tests
