{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types where

import qualified Data.Map as M
import Data.Map (Map)
import Control.Arrow
import Data.List

import qualified Text.PrettyPrint.Leijen as P
import Text.PrettyPrint.Leijen ((<//>), (</>), Pretty, pretty, displayS, renderPretty)

type Power = Double
type Unit = String
type Units = Map Unit Power
type NumUnits = (Double, Units) -- A number, with units

data Expr =
    ENum Double Units | -- A number, with units
    EApply Expr [Expr] | -- Application of a function or operator
    EId String | -- An identifier
    EFn [String] Expr -- A function
    deriving (Show)

data Value =
    VNum Double Units |
    VBuiltin String | -- A built-in function/operator
    VClosure [String] Expr [Map String Value]
    deriving (Show)

instance Eq Value where
    VNum a aUnits == VNum b bUnits = a == b && aUnits == bUnits
    VBuiltin a == VBuiltin b = a == b
    _ == _ = False

data UnitType = UNormal | USI | UBin deriving (Show)

data UnitDef = UnitDef {
    unitType :: UnitType,
    unitNames :: [String],
    unitAbbrs :: [String],
    unitValue :: Maybe NumUnits
} deriving (Show)

data ReplCmd = RStmt Stmt | RLoad String

data Stmt = SUnitDef UnitType [String] [String] (Maybe Expr) | SExpr Expr | SDef String Expr
    deriving (Show)

type ErrorM t = Either String t
err = Left

data Env = Env {
    envUnits :: UnitList,
    envUnitNames :: [String],
    envUnitMap :: UnitMap,
    envDeclarations :: Map String Value,
    envVars :: [Map String Value]
} deriving (Show)

type UnitList = [UnitDef]
type UnitMap = Map String NumUnits

emptyEnv = Env {envUnits = [], envUnitNames = [],
    envUnitMap = M.empty, envDeclarations = M.empty, envVars = [M.empty]}

--- Pretty printing

outputToString doc = displayS (renderPretty 1.0 72 doc) ""
prettyPrint :: Pretty a => a -> String
prettyPrint = outputToString . pretty

instance Pretty Expr where
    pretty (ENum num units)
        | units == M.empty = pretty num
        | otherwise = pretty num </> prettyPrintUnits (M.toList units)
    -- This should never happen
    pretty x = pretty (show x)

instance Pretty Value where
    pretty (VNum num units)
        | units == M.empty = pretty num
        | otherwise = pretty num </> prettyPrintUnits (M.toList units)
    -- This should never happen
    pretty x = pretty (show x)

prettyPrintUnit (unit, 1) = pretty unit
prettyPrintUnit (unit, n) = pretty unit <//> pretty "^" <//> pretty n

prettyPrintUnits units
    | not (null pos) && length neg == 1 = P.hsep (map prettyPrintUnit pos) <//>
        pretty "/" <//> prettyPrintUnit (second negate $ head neg)
    | otherwise = P.hsep (map prettyPrintUnit units)
    where
        (pos, neg) = partition ((>0).snd) units

instance Pretty Units where
    pretty units
        | M.null units = pretty "(unitless)"
        | otherwise = prettyPrintUnits (M.toList units)
