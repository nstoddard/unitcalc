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


evalStmts :: [Stmt] -> Env -> ErrorM Env
evalStmts [] env = pure env
evalStmts (stmt:stmts) env = case evalStmt stmt env of
    Left err -> Left err
    Right (_,env') -> evalStmts stmts env'


evalStmt :: Stmt -> Env -> ErrorM (Maybe Expr, Env)
evalStmt (SUnitDef utype names abbr expr) env = do
    val <- case expr of
        Nothing -> pure Nothing
        Just expr -> Just <$> evalExpr expr env True
    val' <- case val of
        Just (ENum num units) -> pure $ Just (num, units)
        Nothing -> pure Nothing
        x -> err $ "Can't define a unit with the value " ++ prettyPrint x
    let
        unitDef = UnitDef {unitType = utype,
            unitNames = names, unitAbbr = abbr, unitValue = val'}
    pure (Nothing, foldl' addUnitDef env (addSIPrefixes unitDef))
evalStmt (SDef id val) env = do
    val' <- evalExpr val env True
    case val' of
        ENum num units -> pure (Just val', env {
            envVars = M.insert id (num,units) (envVars env)
        })
        x -> err $ "Can't define a variable with the value " ++ prettyPrint x
evalStmt (SExpr expr) env = do
    res <- evalExpr expr env True
    pure (Just res, env)


evalExpr :: Expr -> Env -> Bool -> ErrorM Expr
evalExpr x@(ENum {}) env toBase = pure x
evalExpr (EApply fn args) env toBase = do
    fn' <- evalExpr fn env toBase
    args' <- mapM (\x -> evalExpr x env toBase) args
    case fn' of
        EBuiltin str -> applyBuiltin str args'
        n@(ENum {}) -> foldM (\a b -> applyBuiltin "*" [a,b]) n args'
        x -> err $ "Trying to treat " ++ prettyPrint x ++ " as a function."
evalExpr (EId str) env toBase
    | str `elem` builtins = pure (EBuiltin str)
    | str `M.member` envVars env = let (num,units) = envVars env M.! str in
      pure (ENum num units)
    | str `elem` envUnitNames env = let
        (num,units) = if toBase
            then toBaseUnits (envUnits env) (envUnitMap env)
                (1.0, M.singleton str 1.0)
            else (1.0, M.singleton str 1.0) in
        pure (ENum num units)
    | otherwise = err $ "Unknown identifier or unit: " ++ str ++ "."
evalExpr (EConvert a b) env toBase = do
    a' <- evalExpr a env toBase
    b' <- evalExpr b env False
    b' <- case b' of
        ENum num units -> pure (num, units)
        x -> err $ "Invalid conversion; can't convert to " ++ prettyPrint x
    case (a', validUnit (snd b') env) of
        (ENum num units, True) -> do
            (num',units') <- convertUnits (envUnits env) (envUnitMap env) (num/fst b', units) (snd b')
            pure (ENum num' units')
        (ENum num units, False) -> err $
            "Invalid unit in convesion: " ++ prettyPrint b ++"."
        (x, _) -> err $
            "Invalid conversion: can't convert " ++ prettyPrint x ++ "."
evalExpr (EBuiltin str) env toBase = err
    "Trying to evaluate EBuiltin. This is a bug."

validUnit :: Units -> Env -> Bool
validUnit units env = all (`elem` envUnitNames env) (map fst $ M.toList units)

toBaseUnits :: UnitList -> UnitMap -> NumUnits -> NumUnits
toBaseUnits unitList m (n, units) = M.foldl'
    (\(aN, aUnits) (bN, bUnits) -> (aN*bN, combineUnits aUnits bUnits))
    (n, M.empty) (M.mapWithKey toBaseUnits' units) where
    toBaseUnits' :: Unit -> Power -> NumUnits
    toBaseUnits' unit power = case M.lookup unit m of
        Nothing -> (1.0, M.singleton unit' power) where
            unit' = head . unitNames $ fromJust
                (find (\x -> unit `elem` unitNames x || Just unit == unitAbbr x) unitList)
        Just res -> toBaseUnits unitList m (((**power) *** M.map (*power)) res)

convertUnits :: UnitList -> UnitMap -> NumUnits -> Units -> ErrorM NumUnits
convertUnits unitList m a b
    | aUnits == bUnits = pure (aRes*recip bRes, b)
    | otherwise = err $ "Invalid unit conversion from " ++
        prettyPrint aUnits ++ " to " ++ prettyPrint b ++ "."
    where
    (aRes, aUnits) = toBaseUnits unitList m a
    (bRes, bUnits) = toBaseUnits unitList m (1.0, b)

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
            unitAbbr = (shortPrefix++) <$> unitAbbr unitDef,
            unitValue = Just (mul, M.singleton (head $ unitNames unitDef) 1.0)
        }

siPrefixes = [
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
binPrefixes = [
    ("yobi", "Yi", 1024**8),
    ("zebi", "Zi", 1024**7),
    ("exbi", "Ei", 1024**6),
    ("pebi", "Pi", 1024**5),
    ("tebi", "Ti", 1024**4),
    ("gibi", "Gi", 1024**3),
    ("mebi", "Mi", 1024**2),
    ("kibi", "Ki", 1024**1)
    ]


addUnitDef env unitDef
    | not (unitExists env unitDef) = env {
        envUnits = unitDef : envUnits env,
        envUnitNames = unitNames unitDef ++ F.toList (unitAbbr unitDef) ++ envUnitNames env,
        envUnitMap = case unitValue unitDef of
            Nothing -> envUnitMap env
            Just value -> M.fromList (map (, value) (unitNames unitDef ++ F.toList (unitAbbr unitDef))) `M.union` envUnitMap env
        }
    | otherwise = env --TODO: in this case, tell the user that the unit wasn't actually defined

unitExists env unitDef = any (`elem` envUnitNames env) (unitNames unitDef ++ F.toList (unitAbbr unitDef))


-- Built-in functions and operators
applyBuiltin :: String -> [Expr] -> ErrorM Expr
applyBuiltin "+" [ENum a aUnits, ENum b bUnits]
    | aUnits == bUnits = pure (ENum (a+b) aUnits)
    | otherwise = err "Incompatible units"
applyBuiltin "-" [ENum a aUnits, ENum b bUnits]
    | aUnits == bUnits = pure (ENum (a-b) aUnits)
    | otherwise = err "Incompatible units"
applyBuiltin "-" [ENum a aUnits] = pure (ENum (-a) aUnits)
applyBuiltin "*" [ENum a aUnits, ENum b bUnits] = pure $ ENum (a*b) (combineUnits aUnits bUnits)
applyBuiltin "/" [ENum a aUnits, ENum b bUnits] = pure $ ENum (a/b) (combineUnits aUnits (M.map negate bUnits))
applyBuiltin "^" [ENum a aUnits, ENum b bUnits]
    | M.null bUnits = pure $ ENum (a**b) (M.map (*b) aUnits)
    | otherwise = err "Invalid use of ^"
applyBuiltin f _ = err $ "Invalid call to builtin function " ++
    prettyPrint f

-- TODO: find a way to automatically generate this
builtins = ["+", "-", "*", "/", "^"]
