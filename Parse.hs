module Parse where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

import Text.Parsec hiding ((<|>), many, optional, State, newline)
import Text.Parsec.Expr

import Types


parseReplCmd = whitespace *> (parseLoad <|> parseReset <|> (RStmt <$> parseStmt)) <* whitespace
parseLoad = RLoad <$> (tryString "load" /> some (satisfy (not . isSpace)))
parseReset = tryString "reset" *> pure RReset


parseInput :: String -> String -> Parsec String () a -> ErrorM a
parseInput srcName input parser = case parse (parseWholeInput parser) srcName input of
        Left err_ -> err ("Syntax error " ++ show err_)
        Right expr -> pure expr


parseStmts = anyWhitespace *> many (parseStmt <* anyWhitespace)

parseStmt :: Parsec String () Stmt
parseStmt = parseUnitDef <|> try parseDef <|> (SExpr <$> parseExpr)
parseUnitDef = do
    si <- (tryString "si-unit" *> pure True) <|>
        (tryString "unit" *> pure False)
    names <- whitespace *> sepBy1 identifier (char '/')
    whitespace
    abbr <- option Nothing $ Just <$> try (char '(' /> identifier </ char ')')
    whitespace
    value <- option Nothing $ Just <$> (char '=' /> parseExpr <* whitespace)
    pure $ SUnitDef si names abbr value
parseDef = SDef <$> identifier <*> (whitespace *> char '=' /> parseExpr)


parseExpr :: Parsec String () Expr
parseExpr = do
    res <- buildExpressionParser opTable1 parseExpr'
    convert <- option Nothing $ Just <$> try (whitespace *> tryString "->" />
        parseExpr <* whitespace)
    case convert of
        Nothing -> pure res
        Just units -> pure (EConvert res units)

parseExpr' = try parseApply <|> parsePrefixOp <|> parseExpr''
parseExpr'' = buildExpressionParser opTable2 parseSingleTokenExpr
parseSingleTokenExpr = parseParens <|> parseNum <|> parseId
parseParens = char '(' /> parseExpr </ char ')'
parseId = EId <$> identifier
parseNum = ENum <$> (float <|> fromIntegral <$> decimal) <*> pure M.empty
parseApply = EApply <$> parseExpr'' <*> some (try $ whitespace *> parseExpr'')
parsePrefixOp = EApply <$> (EId <$> prefixOperator) <*> ((:[]) <$> parseExpr)


operatorChars = "/<>?:\\|~!@#$%^&*+-="
reservedOps = [commentLine, commentStart, commentEnd]
keywords = ["unit", "si-unit", "exit", "load"]



--- Lower-level combinators

tryString = try . string
parseWholeInput parser = parser <* eof

whitespace = skipWhitespace " \t"
anyWhitespace = skipWhitespace " \t\n"
someWhitespace = skipReqWhitespace " \t"

infixl 4 </>, />, </
a /> b = a *> whitespace *> b
a </ b = a <* whitespace <* b
a </> b = a <*> (whitespace *> b)

skipWhitespace :: String -> Parsec String () ()
skipWhitespace chars = skipMany (void (oneOf chars) <|> oneLineComment <|>
    multiLineComment <|> void (string "\\\n")) <?> "whitespace"

skipReqWhitespace :: String -> Parsec String () ()
skipReqWhitespace chars = skipSome (void (oneOf chars) <|> oneLineComment <|>
    multiLineComment <|> void (string "\\\n")) <?> "whitespace"

commentLine = "//"
commentStart = "/*"
commentEnd = "*/"

oneLineComment = do
  tryString commentLine
  skipMany (satisfy (/= '\n'))

multiLineComment = do
  tryString commentStart
  inComment

inComment =
  (tryString commentEnd >> pure ()) <|>
  (multiLineComment >> inComment) <|>
  (skipSome (noneOf startEnd) >> inComment) <|>
  (oneOf startEnd >> inComment)
  where
    startEnd = nub $ commentEnd ++ commentStart


skipSome = skipMany1

float = try floating <?> "float"

floating = fractExponent =<< decimal

fractExponent n = fractExponent' <|> exponentOnly where
    fractExponent' = do
        fract <- fraction
        expo <- option 1.0 exponent'
        pure ((fromInteger n + fract)*expo)
    exponentOnly = do
        expo <- exponent'
        pure (fromInteger n * expo)

exponent' = do
    oneOf "eE"
    power <$> decimal where
    power e
        | e < 0 = 1.0/power(-e)
        | otherwise = fromInteger (10^e)

fraction = do
    char '.'
    digits <- some digit
    pure (foldr op 0.0 digits)
    where op d f = (f + fromIntegral (digitToInt d))/10.0

decimal = number 10 digit

number base baseDigit = do
    digits <- some baseDigit
    let n = foldl' (\x d -> base*x + toInteger (digitToInt d)) 0 digits
    seq n (pure n)


backquoteIdentifier = char '`' *> many (noneOf "`") <* char '`'

identStart = satisfy isAlpha
identChar = satisfy (\x -> isAlphaNum x || x=='\'')

identifier = backquoteIdentifier <|> (do
    val <- (:) <$> identStart <*> many identChar
    if val `elem` keywords then mzero else pure val) <?> "identifier"



--- Operators

ops = [
    [("^", AssocRight)],
    [("*", AssocLeft), ("/", AssocLeft)],
    [("+", AssocLeft), ("-", AssocLeft)]
    ]

opTable1 = map (map $ op True) ops
opTable2 = map (map $ op False) ops

op reqSpaces (str,assoc) = binop str reqSpaces assoc
binop str reqSpaces = Infix (try $ do
    when reqSpaces someWhitespace
    name <- operator str False
    when reqSpaces someWhitespace
    pure (\a b -> EApply (EId name) [a, b])
    )

operator str rassoc = (do
    str <- tryString str
    if rassoc /= (last str == ':') || str `elem` reservedOps then mzero
        else pure str
    ) <?> "operator"

prefixOperator = (do
    val <- some $ satisfy (`elem` operatorChars)
    if val `elem` reservedOps then mzero else pure val) <?> "operator"
