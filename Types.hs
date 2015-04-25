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
type NumUnits = (Double, Units)

data Expr = ENum Double Units | EApply Expr [Expr] | EId String |
    EBuiltin String | EConvert Expr Expr
    deriving (Show)

data UnitDef = UnitDef {
    -- Whether it's an SI unit; whether it should allow prefixes like "kilo"
    unitSI :: Bool,
    unitNames :: [String],
    unitAbbr :: Maybe String,
    unitValue :: Maybe NumUnits
} deriving (Show, Read)

data Stmt = SUnitDef Bool [String] (Maybe String) (Maybe Expr) | SExpr Expr | SDef String Expr
    deriving (Show)

type UnitList = [UnitDef]
type UnitMap = Map String NumUnits

type ErrorM t = Either String t
err = Left

data Env = Env {
    envUnits :: UnitList,
    envUnitNames :: [String],
    envUnitMap :: UnitMap,
    envVars :: Map String NumUnits
} deriving (Show, Read)


outputToString doc = displayS (renderPretty 1.0 72 doc) ""
prettyPrint :: Pretty a => a -> String
prettyPrint = outputToString . pretty

instance Pretty Expr where
    pretty (ENum num units)
        | units == M.empty = pretty num
        | otherwise = pretty num </> prettyUnits (M.toList units)
    -- This should never happen
    pretty x = pretty (show x)

prettyUnit (unit, 1) = pretty unit
prettyUnit (unit, n) = pretty unit <//> pretty "^" <//> pretty n

prettyUnits units
    | length pos > 0 && length neg == 1 = P.hsep (map prettyUnit pos) <//>
        pretty "/" <//> prettyUnit (second negate $ head neg)
    | otherwise = P.hsep (map prettyUnit units)
    where
        (pos, neg) = partition ((>0).snd) units

instance Pretty Units where
    pretty units
        | M.null units = pretty "(unitless)"
        | otherwise = prettyUnits (M.toList units)


