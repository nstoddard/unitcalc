{-# LANGUAGE TupleSections #-}

module Eval where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Foldable as F
import Data.Maybe
import Control.Arrow

import Types
import Parse


evalStmts :: [Stmt] -> Env -> ErrorM Env
evalStmts [] env = pure env
evalStmts (stmt:stmts) env = case evalStmt stmt env of
    Left err -> Left err
    Right (_,env') -> evalStmts stmts env'


evalStmt :: Stmt -> Env -> ErrorM (Maybe Value, Env)
evalStmt (SUnitDef utype names abbrs expr) env = do
    val <- case expr of
        Nothing -> pure Nothing
        Just expr -> Just <$> evalExpr expr env
    val' <- case val of
        Just (VNum num units) -> pure $ Just (num, units)
        Nothing -> pure Nothing
        x -> err $ "Can't define a unit with the value " ++ prettyPrint x
    let
        unitDef = UnitDef {unitType = utype,
            unitNames = names, unitAbbrs = abbrs, unitValue = val'}
    env' <- foldM addUnitDef env (addSIPrefixes unitDef)
    pure (Nothing, env')
evalStmt (SDef id val) env = do
    val' <- evalExpr val env
    pure (Just val', env {
        envDeclarations = M.insert id val' (envDeclarations env)
    })
evalStmt (SExpr expr) env = do
    res <- evalExpr expr env
    pure (Just res, env)

lookupVar str [] = undefined
lookupVar str (env:envs) = fromMaybe (lookupVar str envs) (M.lookup str env)

evalExpr :: Expr -> Env -> ErrorM Value
evalExpr (ENum num units) env = pure (VNum num units)
evalExpr (EApply fn args) env = do
    fn' <- evalExpr fn env
    args' <- mapM (`evalExpr` env) args
    case fn' of
        VBuiltin str -> applyBuiltin str args' env
        n@VNum {} -> foldM (\a b -> applyBuiltin "*" [a,b] env) n args'
        VClosure params body closure -> if length params /= length args'
            then err "Params and args must be the same length."
            else do
                let args = zip params args'
                let env' = env {
                    envVars = M.fromList args : closure
                }
                evalExpr body env'
evalExpr (EId str) env
    | str `elem` builtins = pure (VBuiltin str)
    | any (str `M.member`) (envVars env) = pure $ lookupVar str (envVars env)
    | str `M.member` envDeclarations env = pure $ lookupVar str [envDeclarations env]
    | str `elem` envUnitNames env = pure (VNum 1.0 (M.singleton str 1.0))
    | otherwise = err $ "Unknown identifier or unit: " ++ str ++ "."
evalExpr (EFn args body) env = pure $ VClosure args body (envVars env)

validUnits :: Units -> Env -> Bool
validUnits units env = all (`elem` envUnitNames env) (map fst $ M.toList units)

toBaseUnits :: NumUnits -> Env -> NumUnits
toBaseUnits (n, units) env = M.foldl'
    (\(aN, aUnits) (bN, bUnits) -> (aN*bN, combineUnits aUnits bUnits))
    (n, M.empty) (M.mapWithKey toBaseUnits' units) where
    unitList = envUnits env
    m = envUnitMap env
    toBaseUnits' :: Unit -> Power -> NumUnits
    toBaseUnits' unit power = case M.lookup unit m of
        Nothing -> (1.0, M.singleton unit' power) where
            unit' = head . unitNames $ fromJust
                (find (\x -> unit `elem` unitNames x || unit `elem` unitAbbrs x) unitList)
        Just res -> toBaseUnits (((**power) *** M.map (*power)) res) env

convertUnits :: NumUnits -> Units -> Env -> ErrorM NumUnits
convertUnits a b env
    | aUnits == bUnits = pure (aRes*recip bRes, b)
    | otherwise = err $ "Invalid unit conversion from " ++
        prettyPrint aUnits ++ " to " ++ prettyPrint b ++ "."
    where
    (aRes, aUnits) = toBaseUnits a env
    (bRes, bUnits) = toBaseUnits (1.0, b) env

combineUnits :: Units -> Units -> Units
combineUnits = M.mergeWithKey (\_ a b -> if a+b==0 then Nothing else Just (a+b)) id id

addSIPrefixes unitDef = case unitType unitDef of
    USI -> unitDef : map addSIPrefixes' siPrefixes
    UBin -> unitDef : map addSIPrefixes' siPrefixes ++ map addSIPrefixes' binPrefixes
    UNormal -> [unitDef]
    where
        addSIPrefixes' (prefix, shortPrefix, mul) = UnitDef {
            unitType = UNormal,
            unitNames = (prefix++) <$> unitNames unitDef,
            unitAbbrs = (shortPrefix++) <$> unitAbbrs unitDef,
            unitValue = Just (mul, M.singleton (head $ unitNames unitDef) 1.0)
        }

siPrefixes =
    [ ("yotta", "Y", 1000**8)
    , ("zetta", "Z", 1000**7)
    , ("exa"  , "E", 1000**6)
    , ("peta" , "P", 1000**5)
    , ("tera" , "T", 1000**4)
    , ("giga" , "G", 1000**3)
    , ("mega" , "M", 1000**2)
    , ("kilo" , "k", 1000**1)
    , ("hecto", "h", 100)
    , ("deca" , "da", 10)

    , ("deci" , "d", 0.1)
    , ("centi", "c", 0.01)
    , ("milli", "m", 1000**(-1))
    , ("micro", "mu",1000**(-2))
    , ("nano" , "n", 1000**(-3))
    , ("pico" , "p", 1000**(-4))
    , ("femto", "f", 1000**(-5))
    , ("atto" , "a", 1000**(-6))
    , ("zepto", "z", 1000**(-7))
    , ("yocto", "y", 1000**(-8))
    ]
binPrefixes =
    [ ("yobi", "Yi", 1024**8)
    , ("zebi", "Zi", 1024**7)
    , ("exbi", "Ei", 1024**6)
    , ("pebi", "Pi", 1024**5)
    , ("tebi", "Ti", 1024**4)
    , ("gibi", "Gi", 1024**3)
    , ("mebi", "Mi", 1024**2)
    , ("kibi", "Ki", 1024**1)
    ]


addUnitDef env unitDef
    | not (unitExists env unitDef) = pure $ env {
        envUnits = unitDef : envUnits env,
        envUnitNames = unitNames unitDef ++ unitAbbrs unitDef ++ envUnitNames env,
        envUnitMap = case unitValue unitDef of
            Nothing -> envUnitMap env
            Just value -> M.fromList (map (, value) (unitNames unitDef ++ unitAbbrs unitDef)) `M.union` envUnitMap env
        }
    | otherwise = err $ "Unit already exists: " ++ (head $ unitNames unitDef) ++ "."

unitExists env unitDef = any (`elem` envUnitNames env) (unitNames unitDef ++ unitAbbrs unitDef)


-- Built-in functions and operators
applyBuiltin :: String -> [Value] -> Env -> ErrorM Value
applyBuiltin "@" [a, b] env = do
    b <- case b of
        VNum num units | num == 1.0 -> pure units
        x -> err $ "Invalid conversion; can't convert to " ++ prettyPrint x ++ "."
    case (a, validUnits b env) of
        (VNum num units, True) -> do
            (num',units') <- convertUnits (num, units) b env
            pure (VNum num' units')
        (VNum num units, False) -> err $
            "Invalid unit in conversion: " ++ prettyPrint b ++ "."
        (x, _) -> err $
            "Invalid conversion: can't convert " ++ prettyPrint x ++ "."
applyBuiltin "+" [VNum a aUnits, VNum b bUnits] env = do
    (b', bUnits') <- convertUnits (b, bUnits) aUnits env
    if aUnits == bUnits' then pure (VNum (a+b') aUnits)
        else err "Incompatible units"
applyBuiltin "-" [VNum a aUnits, VNum b bUnits] env = do
    (b', bUnits') <- convertUnits (b, bUnits) aUnits env
    if aUnits == bUnits' then pure (VNum (a-b') aUnits)
        else err "Incompatible units"
applyBuiltin "-" [VNum a aUnits] _ = pure (VNum (-a) aUnits)
applyBuiltin "*" [VNum a aUnits, VNum b bUnits] _ = pure $ VNum (a*b) (combineUnits aUnits bUnits)
applyBuiltin "/" [VNum a aUnits, VNum b bUnits] _ = pure $ VNum (a/b) (combineUnits aUnits (M.map negate bUnits))
applyBuiltin "^" [VNum a aUnits, VNum b bUnits] _
    | M.null bUnits = pure $ VNum (a**b) (M.map (*b) aUnits)
    | otherwise = err "Invalid use of ^"
applyBuiltin "sin" [VNum a aUnits] _
    | M.null aUnits = pure $ VNum (sin a) aUnits
    | otherwise = err "'sin' only works for unitless quantities"
applyBuiltin "cos" [VNum a aUnits] _
    | M.null aUnits = pure $ VNum (cos a) aUnits
    | otherwise = err "'cos' only works for unitless quantities"
applyBuiltin "tan" [VNum a aUnits] _
    | M.null aUnits = pure $ VNum (tan a) aUnits
    | otherwise = err "'tan' only works for unitless quantities"
applyBuiltin "ln" [VNum a aUnits] _
    | M.null aUnits = pure $ VNum (log a) aUnits
    | otherwise = err "'ln' only works for unitless quantities"
applyBuiltin f _ _ = err $ "Invalid call to builtin function " ++
    prettyPrint f

builtins :: [String]
builtins = concatMap (map fst) ops ++ ["sin", "cos", "tan", "ln"]
