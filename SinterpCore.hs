module SinterpCore(Identifier,
                   Binding,
                   Environment,
                   Compare(..), 
                   Numeric(..),
                   Value(..),
                   Expr(..)) where

type Identifier = String

type Binding = (Identifier, Value)

type Environment = [Binding]

data Compare = GT
             | LT
             | EQ
               deriving (Show, Eq)

data Numeric = IntN Integer
             | FloatN Double
               deriving (Show, Eq)

  
data Value = NumberV Numeric
           | BooleanV Bool
           | FunV Identifier Expr Environment
           | ListV [Value]
               deriving (Show, Eq)  

data Expr = Number Numeric
          | Boolean Bool
          | And (Expr, Expr)
          | Or (Expr, Expr)
          | Not (Expr)
          | Add (Expr, Expr)
          | Sub (Expr, Expr)
          | Mul (Expr, Expr)
          | Div (Expr, Expr)
          | If Expr Expr Expr
          | Cmp Compare Expr Expr
          | Let (Identifier, Expr) Expr
          | Id Identifier
          | Fun Identifier Expr
          | Apply Expr Expr
          | Empty
          | Cons Expr Expr
          | Head Expr
          | Tail Expr
          | IsEmpty Expr
            deriving (Show, Eq)