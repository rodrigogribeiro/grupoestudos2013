> module Parser where

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr

> data Exp = Const Int
>          | Add Exp Exp
>          | Mult Exp Exp
>          deriving (Eq, Ord, Show)

> expr :: Parser Exp
> expr = buildExpressionParser table factor <?> "expression"

> table :: [[ Operator Char st Exp ]]
> table = [[ op "*" Mult AssocLeft],
>          [ op "+" Add  AssocLeft]]
>         where
>           op s f assoc = Infix (do {string s ; return f}) assoc

> factor = do { char '(' ; x <- expr ; char ')' ; return x }
>          <|> number
>          <?> "simple expression"
  
> number :: Parser Exp
> number = do { ds <- many1 digit; return (Const (read ds)) } <?> "number"
