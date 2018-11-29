module Parse where

import qualified Data.Map as M
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

import Text.Parsec hiding ((<|>), many, optional, State, newline)
import Text.Parsec.Expr

import Types


parseReplCmd = whitespace *> (parseLoad <|> (RStmt <$> parseStmt)) <* whitespace
parseLoad = RLoad <$> (tryString "load" /> some (satisfy (not . isSpace)))


parseInput :: String -> String -> Parsec String () a -> ErrorM a
parseInput srcName input parser = case parse (parseWholeInput parser) srcName input of
        Left err_ -> err ("Syntax error " ++ show err_)
        Right expr -> pure expr


parseStmts = anyWhitespace *> many (parseStmt <* anyWhitespace)

parseStmt :: Parsec String () Stmt
parseStmt = parseUnitDef <|> try parseDef <|> (SExpr <$> parseExpr)
parseUnitDef = do
    utype <- (tryString "si-unit" *> pure USI) <|>
        (tryString "bin-unit" *> pure UBin) <|>
        (tryString "unit" *> pure UNormal)
    names <- whitespace *> sepBy1 identifier (char '/')
    whitespace
    abbrs <- option [] $ try (char '(' /> sepBy1 identifier (char '/') </ char ')')
    whitespace
    value <- option Nothing $ Just <$> (char '=' /> parseExpr <* whitespace)
    pure $ SUnitDef utype names abbrs value
parseDef = SDef <$> identifier <*> (whitespace *> char '=' /> parseExpr)


parseExpr :: Parsec String () Expr
parseExpr = buildExpressionParser opTable1 parseExpr'

parseExpr' = try parseApply <|> parseFn <|> parseExpr''
parseExpr'' = buildExpressionParser opTable2 parseSingleTokenExpr
parseSingleTokenExpr = parsePrefixOp <|> parseParens <|> parseNum <|> parseId
parseParens = char '(' /> parseExpr </ char ')'
parseId = EId <$> identifier
parseNum = ENum <$> (float <|> fromIntegral <$> integer) <*> pure M.empty
parseApply = EApply <$> parseExpr'' <*> some (try $ whitespace *> parseExpr'')
parsePrefixOp = EApply <$> (EId <$> prefixOperator) <*> ((:[]) <$> parseSingleTokenExpr)
parseFn = do
    args <- tryString "\\" /> sepBy1 identifier (char ',' *> whitespace) <* whitespace
    body <- tryString "->" /> parseExpr
    pure $ EFn args body

operatorChars = "/<>?:\\|~!@#$%^&*+-="
reservedOps = ["#", "\\", "->"]
keywords = ["unit", "si-unit", "bin-unit", "exit", "load"]


--- Operators

ops = [
    [("^", AssocRight)],
    [("*", AssocLeft), ("/", AssocLeft)],
    [("+", AssocLeft), ("-", AssocLeft)],
    [("@", AssocLeft)]
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


--- Lower-level combinators

tryString = try . string
parseWholeInput parser = parser <* eof

infixl 4 </>, />, </
a /> b = a *> whitespace *> b
a </ b = a <* whitespace <* b
a </> b = a <*> (whitespace *> b)

whitespace = whitespace' " \t"
anyWhitespace = whitespace' " \t\n"
someWhitespace = whitespace1' " \t"

whitespace' chars = skipMany (void (oneOf chars) <|> comment)
whitespace1' chars = skipMany1 (void (oneOf chars) <|> comment)
comment = char '#' *> skipMany (noneOf "\n")
    
integer = read . catMaybes <$> some ((Just <$> digit) <|> (char '_' *> pure Nothing))
float = try $ do
    a <- integer
    char '.'
    b <- integer
    pure $ read (show a ++ "." ++ show b)
identifier = (:) <$> letter <*> many alphaNum
