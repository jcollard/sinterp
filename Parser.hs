module Parser(parse) where

import Sinterp
import Control.Monad
import Control.Applicative hiding((<|>), many)
import Text.ParserCombinators.Parsec hiding(parse)
import qualified Text.ParserCombinators.Parsec as P
import Prelude hiding(div, not, and, or, EQ, LT, GT)

parse :: String -> Either String Expr
parse str = let parsed = P.parse expr "" str in
  case parsed of
    Left err -> Left . show $ err
    Right e -> Right e

spaces1 :: Parser ()
spaces1 = skipMany1 space

expr :: Parser Expr
expr = spaces >> (try apply <|> expr')  >>= precedent1 >>= precedent0


expr' :: Parser Expr
expr' = (try paren <|> term) >>= precedent0
  where paren = do {spaces; char '('; e <- expr; char ')'; return e}

apply :: Parser Expr
apply = partialApply >>= partialApply'
 where partialApply = do {e0 <- expr'; spaces; return (Apply e0)}
       partialApply' part = (try recur <|> finalApply) where
         finalApply = do {en <- expr'; spaces; return (part en)}
         recur = do {en <- expr'; spaces;  partialApply' (Apply (part en))}

value :: Parser Expr
value = ((choice . map try $ expressions) <|> identifier) >>= precedent1
  where expressions = [lambda, parseLet, parseIf, parseNot, parseEmpty, parseHead, parseTail, isEmpty, numeric, boolean]

term :: Parser Expr
term = value >>= precedent1

precedent :: [Expr -> Parser Expr] -> Expr -> Parser Expr
precedent operators e = (choice . map try $ (operators <*> [e])) <|> return e

precedent0 = precedent [add, sub, or, gt, lt, eq, cons]
add = operator "+" (curry Add) expr
sub = operator "-" (curry Sub) expr
or = operator "or" (curry Or) expr
cons = operator ":" Cons expr
gt = operator ">" (Cmp GT) expr
lt = operator "<" (Cmp LT) expr
eq = operator "==" (Cmp EQ) expr

precedent1 = precedent [mul, div, and]
mul = operator "*" (curry Mul) expr
div = operator "/" (curry Div) expr
and = operator "and" (curry And) expr


operator :: String -> (Expr -> Expr -> Expr) -> (Parser Expr) -> Expr -> Parser Expr
operator op constructor nextExpr e0 = do 
  string op <* spaces
  e1 <- nextExpr <* spaces
  return (constructor e0 e1)
  
lambda = do 
  char '\\' <* spaces
  id <- identifier' <* spaces
  string "->" <* spaces
  e <- expr <* spaces
  return (Fun id e)

isEmpty = do
  string "isEmpty" <* spaces
  e <- expr <*  spaces 
  return . IsEmpty $ e

parseTail = do 
  string "tail" <*  spaces
  e <- expr <*  spaces
  return . Tail $ e

parseHead = do
  string "head" <*  spaces
  e <- expr <*  spaces
  return . Head $ e

parseEmpty = string "[]" >> spaces >> return Empty

parseNot = do {string "not"; e <- expr; spaces; return (Not e)}

parseIf = do 
  string "if" <* spaces1
  cond <- expr <* spaces
  string "then" <* spaces
  t <- expr <* spaces
  string "else" <* spaces1
  f <- expr <* spaces
  return (If cond t f)

parseLet = do 
  string "let" <* spaces1
  id <- identifier' <* spaces 
  char '=' <* spaces 
  arg <- expr <* spaces
  string "in" <* spaces1 
  body <- expr <* spaces
  return (Let (id, arg) body)

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

identifier = Id <$> identifier'
identifier' = do
  str <- (letter <:> many (try alphaNum <|> char '_'))
  spaces
  if elem str keywords 
    then unexpected ("reserved keyword: " ++ str)
    else return str
  
keywords = ["let", "in", "if", "then", "else"]  
  
boolean = try true <|> false
true = true' >> return (Boolean True)
true' = string "true" <* spaces
false = false' >> return (Boolean False)
false' = string "false" <* spaces

integer = Number . IntN . read <$> integer'
float = Number . FloatN . read <$> float'
numeric = try float <|> integer
number' = many1 digit <* spaces
integer' = plus <|> minus <|> number' where
  plus = char '+' <:> number'
  minus = char '-' <:> number'
float' = integer' <++> (try decimal <|> eff) where
  decimal = char '.' <:> number'
  eff = char 'f' >> spaces >> return ""
