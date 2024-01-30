-- Put your parser implementation in this file
module ParserImpl where

import Types

-- import ReadP or Parsec as relevant
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Types as T

parseExp :: String -> Either String Exp
parseExp input = case parse (expParser <* eof) "" input of
    Left err -> Left $ show err
    Right expr -> Right expr

-- Main parser for expressions (Exp)
expParser :: Parser T.Exp
expParser = try rollParser <|> try letParser <|> additionParser <|> sExpParser

letParser :: Parser T.Exp
letParser = do
  spaces
  string "let"
  spaces
  varName <- vNameParser
  spaces
  string "be"
  spaces
  exp1 <- sExpParser
  option (T.Let varName exp1 Nothing) (letInExpParser varName exp1)

letInExpParser :: String -> T.Exp -> Parser T.Exp
letInExpParser varName exp1 = do
  string "in"
  spaces
  T.Let varName exp1 . Just <$> expParser

vNameParser :: Parser String
vNameParser = many1 letter

rollParser :: Parser T.Exp
rollParser = do
    string "roll"
    spaces
    expr <- exprInParentheses <|> (T.Var <$> vNameParser)
    return $ T.Roll expr

-- Define a separate parser for expressions within parentheses
exprInParentheses :: Parser T.Exp
exprInParentheses = between
    (char '(' >> spaces)
    (spaces >> char ')' >> spaces)
    expParser


-- Parser for simple expressions (SExp)
sExpParser :: Parser T.Exp
sExpParser = try additionParser <|> basicSExpParser

-- Addition operator '+'
additionParser :: Parser T.Exp
additionParser = comparisonLevelParser `chainl1` addOp

-- Addition operation parser
addOp :: Parser (T.Exp -> T.Exp -> T.Exp)
addOp = do
    spaces
    char '+'
    spaces
    return (\exp1 exp2 -> T.Sum (T.Join [exp1, exp2]))

basicSExpParser :: Parser T.Exp
basicSExpParser = try numParser
               <|> try takeParser
               <|> try countParser
               <|> try sumParser 
               <|> try varParser
               <|> try joinParser
               <|> try timesParser
               <|> try comparisonParser


numParser :: Parser T.Exp
numParser = do
  spaces
  num <- many1 digit
  spaces
  return $ T.Cst (read num)

varParser :: Parser T.Exp
varParser = try $ do
  spaces
  var <- many1 letter
  spaces
  return $ T.Var var

sumParser :: Parser T.Exp
sumParser = do
  spaces
  string "sum"
  spaces
  T.Sum <$> additionWithoutSumParser

-- New parser for handling addition without wrapping in Sum
additionWithoutSumParser :: Parser T.Exp
additionWithoutSumParser = comparisonLevelParser `chainl1` addOpWithoutSum

-- Modified addition operation parser without Sum
addOpWithoutSum :: Parser (T.Exp -> T.Exp -> T.Exp)
addOpWithoutSum = do
    spaces
    char '+'
    spaces
    return (\exp1 exp2 -> T.Join [exp1, exp2])

countParser :: Parser T.Exp
countParser = do
  spaces
  string "count"
  spaces
  T.Count <$> sExpParser

-- 'times' operator (right associative)
timesParser :: Parser T.Exp
timesParser = do
  exp1 <- basicSExpParser
  spaces
  string "times"
  spaces
  exp2 <- expParser -- Right associativity: allow timesParser to be nested on the right
  return $ T.Times exp1 exp2

-- Intermediate precedence level for 'times'
timesLevelParser :: Parser T.Exp
timesLevelParser = try timesParser <|> basicSExpParser

takeParser :: Parser T.Exp
takeParser = do
  try (string "take") -- Use 'try' for backtracking if needed
  spaces
  sel <- (string "highest" >> return T.Max) <|> (string "lowest" >> return T.Min) <|> (string "any" >> return T.Rand)
  spaces
  string "from"
  spaces
  case sel of
    T.Max -> T.Take Max <$> vNameParser
    _ -> T.Take sel <$> numParserC

numParserC :: Parser String
numParserC = do
  spaces
  num <- many1 digit
  spaces
  return num

-- Comparison operators ('is', 'is not')
comparisonParser :: Parser T.Exp
comparisonParser = do
  exp1 <- timesLevelParser
  spaces
  _ <- try (string "is not") <|> string "is"
  spaces
  Is exp1 <$> timesLevelParser

comparisonLevelParser :: Parser T.Exp
comparisonLevelParser = try comparisonParser <|> timesLevelParser

joinParser :: Parser T.Exp
joinParser = do
    char '('
    exps <- sepBy sExpParser (char ',')
    char ')'
    return $ case exps of
        [] -> T.Join []  -- Empty bag
        _  -> T.Join exps