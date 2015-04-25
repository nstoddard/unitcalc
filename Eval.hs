{-# LANGUAGE NoMonomorphismRestriction, LambdaCase, TupleSections #-}

module Eval where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Foldable as F
import Data.Maybe
import Control.Arrow

import Types


evalStmts :: [Stmt] -> Env -> ErrorM Env
evalStmts [] env = pure env
evalStmts (stmt:stmts) env = case evalStmt stmt env of
    Left err -> Left err
    Right (_,env') -> evalStmts stmts env'


evalStmt :: Stmt -> Env -> ErrorM (Maybe Expr, Env)
evalStmt (SUnitDef si names abbr expr) env = do
    val <- case expr of
        Nothing -> pure Nothing
        Just expr -> Just <$> evalExpr expr env
    val' <- case val of
        Just (ENum num units) -> pure $ Just (num, units)
        Nothing -> pure Nothing
        x -> err $ "Can't define a unit with the value " ++ prettyPrint x
    let
        unitDef = UnitDef {unitSI = si,
            unitNames = names, unitAbbr = abbr, unitValue = val'}
    pure (Nothing, foldl' addUnitDef env (addSI unitDef))
evalStmt (SDef id val) env = do
    val' <- evalExpr val env
    case val' of
        ENum num units -> pure (Just val', env {
            envVars = M.insert id (num,units) (envVars env)
        })
        x -> err $ "Can't define a variable with the value " ++ prettyPrint x
evalStmt (SExpr expr) env = do
    res <- evalExpr expr env
    pure (Just res, env)


evalExpr :: Expr -> Env -> ErrorM Expr
evalExpr x@(ENum {}) env = pure x
evalExpr (EApply fn args) env = do
    fn' <- evalExpr fn env
    args' <- mapM (`evalExpr` env) args
    case fn' of
        EBuiltin str -> applyBuiltin str args'
        n@(ENum {}) -> foldM (\a b -> applyBuiltin "*" [a,b]) n args'
        x -> err $ "Trying to treat " ++ prettyPrint x ++ " as a function."
evalExpr (EId str) env
    | str `elem` builtins = pure (EBuiltin str)
    | str `M.member` envVars env = let (num,units) = envVars env M.! str in
      pure (ENum num units)
    | str `elem` envUnitNames env = let (num,units) = toBaseUnits (envUnits env) (envUnitMap env) (1.0, M.singleton str 1.0) in
        pure (ENum num units)
    | True = err $ "Unknown identifier: " ++ str ++ "."
evalExpr (EConvert a b) env = do
    a' <- evalExpr a env
    b' <- evalExpr b env
    b' <- case b' of
        ENum 1.0 units -> pure units
        x -> err $ "Invalid conversion; can't convert to " ++ prettyPrint x
    case (a', validUnit b' env) of
        (ENum num units, True) -> do
            (num',units') <- convertUnits (envUnits env) (envUnitMap env) (num, units) b'
            pure (ENum num' units')
        (ENum num units, False) -> err $
            "Invalid unit in convesion: " ++ prettyPrint b ++"."
        (x, _) -> err $
            "Invalid conversion: can't convert " ++ prettyPrint x ++ "."
evalExpr (EBuiltin str) env = err
    "Trying to evaluate EBuiltin. This is a bug."



toBaseUnits :: UnitList -> UnitMap -> NumUnits -> NumUnits
toBaseUnits unitList m (n, units) = M.foldl'
    (\(aN, aUnits) (bN, bUnits) -> (aN*bN, combineUnits aUnits bUnits))
    (n, M.empty) (M.mapWithKey (toBaseUnits' m) units) where
        toBaseUnits' :: UnitMap -> Unit -> Power -> NumUnits
        toBaseUnits' m unit power = case M.lookup unit m of
            Nothing -> (1.0, M.singleton unit' power) where
                unit' = head . unitNames $ fromJust
                    (find (\x -> unit `elem` unitNames x || Just unit == unitAbbr x) unitList)
            Just res -> toBaseUnits unitList m (((**power) *** M.map (*power)) res)

convertUnits :: UnitList -> UnitMap -> NumUnits -> Units -> ErrorM NumUnits
convertUnits unitList m a b
    | aUnits == bUnits = pure (aRes*bRes', b)
    | True = err $ "Invalid unit conversion from " ++ prettyPrint aUnits ++
        " to " ++ prettyPrint b ++ "."
    where
    (aRes, aUnits) = toBaseUnits unitList m a
    (bRes, bUnits) = toBaseUnits unitList m (1.0, b)
    (bRes', bUnits') = (recip bRes, M.map negate bUnits)

combineUnits :: Units -> Units -> Units
combineUnits = M.mergeWithKey (\_ a b -> if a+b==0 then Nothing else Just (a+b)) id id


addSI unitDef
    | unitSI unitDef == True = unitDef : map addSI' si
    | True = [unitDef] where
    addSI' (prefix, shortPrefix, mul) = UnitDef {
        unitSI = False,
        unitNames = (prefix++) <$> unitNames unitDef,
        unitAbbr = (shortPrefix++) <$> unitAbbr unitDef,
        unitValue = Just (mul, M.singleton (head $ unitNames unitDef) 1.0)
    }

addUnitDef env unitDef
    | not (unitExists env unitDef) = env {
        envUnits = unitDef : envUnits env,
        envUnitNames = unitNames unitDef ++ F.toList (unitAbbr unitDef) ++ envUnitNames env,
        envUnitMap = case unitValue unitDef of
            Nothing -> envUnitMap env
            Just value -> M.fromList (map (, value) (unitNames unitDef ++ F.toList (unitAbbr unitDef))) `M.union` envUnitMap env
        }
    | True = env

unitExists env unitDef = any (`elem` envUnitNames env) (unitNames unitDef ++ F.toList (unitAbbr unitDef))

validUnit :: Units -> Env -> Bool
validUnit units env = all (`elem` envUnitNames env) (map fst $ M.toList units)

applyBuiltin :: String -> [Expr] -> ErrorM Expr
applyBuiltin "+" [ENum a aUnits, ENum b bUnits]
    | aUnits == bUnits = pure (ENum (a+b) aUnits)
    | True = err "Incompatible units"
applyBuiltin "-" [ENum a aUnits, ENum b bUnits]
    | aUnits == bUnits = pure (ENum (a-b) aUnits)
    | True = err "Incompatible units"
applyBuiltin "-" [ENum a aUnits] = pure (ENum (-a) aUnits)
applyBuiltin "*" [ENum a aUnits, ENum b bUnits] = pure $ ENum (a*b) (combineUnits aUnits bUnits)
applyBuiltin "/" [ENum a aUnits, ENum b bUnits] = pure $ ENum (a/b) (combineUnits aUnits (M.map negate bUnits))
applyBuiltin "^" [ENum a aUnits, ENum b bUnits]
    | M.null bUnits = pure $ ENum (a**b) (M.map (*b) aUnits)
    | True = err "Invalid use of ^"
applyBuiltin f _ = err $ "Invalid call to builtin function " ++
    prettyPrint f


si = [
    ("yotta", "Y", 1000**8),
    ("zetta", "Z", 1000**7),
    ("exa"  , "E", 1000**6),
    ("peta" , "P", 1000**5),
    ("tera" , "T", 1000**4),
    ("giga" , "G", 1000**3),
    ("mega" , "M", 1000**2),
    ("kilo" , "k", 1000**1),
    ("hecto", "h", 100),
    ("deca" , "da", 10),

    ("deci" , "d", 0.1),
    ("centi", "c", 0.01),
    ("milli", "m", 1000**(-1)),
    ("micro", "mu",1000**(-2)),
    ("nano" , "n", 1000**(-3)),
    ("pico" , "p", 1000**(-4)),
    ("femto", "f", 1000**(-5)),
    ("atto" , "a", 1000**(-6)),
    ("zepto", "z", 1000**(-7)),
    ("yocto", "y", 1000**(-8))
    ]

-- TODO: find a way to automatically generate this
builtins = ["+", "-", "*", "/", "^"]
