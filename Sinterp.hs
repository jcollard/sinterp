module Sinterp(Identifier,
               Binding,
               Environment,
               Compare(..), 
               Numeric(..),
               Value(..),
               Expr(..),
               desugar) where

import SinterpCore(Identifier, Environment, Binding, Compare, Numeric, Value)
import qualified SinterpCore as C

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

desugar' :: (Expr, Expr) -> (C.Expr, C.Expr)
desugar' (e0, e1) = (desugar e0, desugar e1)

desugar :: Expr -> C.Expr
desugar (Number n) = C.Number n
desugar (Boolean b) = C.Boolean b
desugar (And e) = C.And . desugar' $ e
desugar (Or e) = C.Or . desugar' $ e
desugar (Not e) = C.Not . desugar $ e
desugar (Add e) = C.Add . desugar' $ e
desugar (Sub e) = C.Sub . desugar' $ e
desugar (Mul e) = C.Mul . desugar' $ e
desugar (Div e) = C.Div . desugar' $ e
desugar (If e0 e1 e2) = C.If (desugar e0) (desugar e1) (desugar e2)
desugar (Cmp c e0 e1) = C.Cmp c (desugar e0) (desugar e1)
desugar (Let (id, e0) e1) = C.Let (id, desugar e0) (desugar e1)
desugar (Id id) = C.Id id
desugar (Fun id body) = C.Fun id (desugar body)
desugar (Apply e0 e1) = C.Apply (desugar e0) (desugar e1)
desugar Empty = C.Empty
desugar (Cons e0 e1) = C.Cons (desugar e0) (desugar e1)
desugar (Head e) = C.Head . desugar $ e
desugar (Tail e) = C.Tail . desugar $ e
desugar (IsEmpty e) = C.IsEmpty . desugar $ e
