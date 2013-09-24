{-# LANGUAGE BangPatterns #-}

module Evaluator(eval) where

import Prelude hiding(div, EQ, LT, GT)
import qualified Prelude as Prelude
import SinterpCore

class LangNum a where
  add :: a -> a -> a
  sub :: a -> a -> a
  mul :: a -> a -> a
  div :: a -> a -> a
  gt :: a -> a -> Bool
  lt :: a -> a -> Bool
  eq :: a -> a -> Bool

coerce :: (Numeric, Numeric) -> (Numeric, Numeric)
coerce n@(IntN _, IntN _) = n
coerce n@(FloatN _, FloatN _) = n
coerce (IntN x, y@(FloatN _)) = (FloatN (fromIntegral x), y)
coerce (x@(FloatN _), IntN y) = (x, FloatN (fromIntegral y))

numericOp :: (Integer -> Integer -> a) ->
             (Double -> Double -> a) ->
             Numeric -> Numeric -> a
numericOp iOp _ (IntN n0) (IntN n1) = (n0 `iOp` n1)
numericOp _ fOp (FloatN n0) (FloatN n1) = (n0 `fOp` n1)
numericOp iOp fOp x y = (uncurry (numericOp iOp fOp)) . coerce $ (x, y)

toNumOp :: Numericable b => (a -> a -> b) -> a -> a -> Numeric
toNumOp op v0 v1 = --((.) . (.)) toNumeric
  let !newVal = op v0 v1 in toNumeric newVal

instance LangNum Numeric where
  add = numericOp (toNumOp (+)) (toNumOp (+))
  sub = numericOp (toNumOp (-)) (toNumOp (-))
  mul = numericOp (toNumOp (*)) (toNumOp (*))
  div = numericOp (toNumOp (Prelude.div)) (toNumOp (/))
  gt = numericOp (>) (>)
  lt = numericOp (<) (<)
  eq = numericOp (==) (==)

class Numericable a where
  toNumeric :: a -> Numeric
  
instance Numericable Integer where
  toNumeric = IntN
  
instance Numericable Double where
  toNumeric = FloatN
  
instance Numericable Value where
  toNumeric (NumberV n) = n
  toNumeric _ = error "Could not convert non-NumberV Value to Numeric"

toBool :: Value -> Bool
toBool (BooleanV b) = b
toBool _ = error "Could not convert non-BooleanV Value to Bool"

toList :: Value -> [Value]
toList (ListV xs) = xs
toList _ = error "Could not convert non-ListV Value to [Value]"

evalOp :: (Value -> a) -> (a -> Value) -> (Expr -> Value) -> (a -> a -> a) -> (Expr, Expr) -> Value
evalOp coerce constructor eval op (e0, e1) = 
  let eval' = coerce . eval in
  constructor ((eval' e0) `op` (eval' e1))
                                 
evalNum = evalOp toNumeric NumberV
evalBool = evalOp toBool BooleanV

compareNum :: Compare -> Numeric -> Numeric -> Bool
compareNum EQ = eq
compareNum LT = lt
compareNum GT = gt

extend :: (Identifier, Value) -> Environment -> Environment
extend i@(_, NumberV !n) is = i:is
extend i@(_, BooleanV !b) is = i:is
extend i@(_, FunV !id !exp !env) is = i:is
extend i@(_, ListV !xs) is = i:is

eval :: Expr -> Value
eval = eval' []

eval' :: Environment -> Expr -> Value
eval' env e = 
  let eval'' = eval' env in
  case e of
    (Number !n) -> NumberV n
    (Boolean !b) -> BooleanV b
    (And !es) -> evalBool eval'' (&&) es
    (Or !es) -> evalBool eval'' (||) es
    (Not !e) -> BooleanV (not . toBool $ (eval e))
    (Add !es) -> evalNum eval'' add es
    (Sub !es) -> evalNum eval'' sub es
    (Mul !es) -> evalNum eval'' mul es
    (Div !es) -> evalNum eval'' div es
    (If !cond !t_exp !f_exp) ->
      case (toBool . eval'' $ cond) of
        True -> eval'' t_exp
        False -> eval'' f_exp
    (Cmp cmp e0 e1) -> 
      let !eval'' = toNumeric . (eval' env) in
      BooleanV (compareNum cmp (eval'' e0) (eval'' e1))
    (Let (!x, !exp) !body) ->
      let !x_env = extend (x, eval' env exp) env in
      eval' x_env body
    (Id !x) ->
      case (lookup x env) of
        Just !val -> val
        Nothing -> error ("Unbound identifier " ++ x ++ " encountered.")
    (Fun !x !body) -> FunV x body env
    (Apply !func !arg) ->
      case (eval'' func) of
        FunV !x !body !f_env -> 
          let !x_env = extend (x, eval'' arg) f_env in
          eval' x_env body
        _ -> error "Could not apply non-function value."
    Empty -> ListV []
    Cons !e0 !e1 -> ListV $ (eval'' e0):(toList . eval'' $ e1)
    Head !e -> head . toList . eval'' $ e
    Tail !e -> ListV $ tail . toList . eval'' $ e
    IsEmpty !e -> BooleanV $ (ListV []) == (eval'' e)