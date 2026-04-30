module TypeChecker where

import AST

-- Either is a pre-defined data type in Haskell.
-- It is often used to deal with computations that might fail, and
-- is defined as:
--
-- data Either a b = Left a
--                 | Right b
--
-- Either is also an instance of Monad. Remember, a Monad
-- is a triple (M a, >>=, return), where M a is any parametric
-- type.
--
-- The Either Monad is likely implemented as:
--
-- instance Monad (Either a) where
--   return = Right
--   Left v >>= f = Left v
--   Right v >>= f = f v
--
-- Our design is to benefit from the Either monad to deal with
-- the situation that a type checker might eventually fail.

type Res = Either String

throw :: String -> Either String a
throw = Left

checker :: Expr -> Res Type
checker expr = case expr of
  ETrue -> return TBool
  EFalse -> return TBool

  If e1 e2 e3 ->
    checker e1 >>= \t1 ->
    checker e2 >>= \t2 ->
    checker e3 >>= \t3 ->
    if t1 == TBool
    then if t2 == t3 then return t2 else throw ("then/else branches have different types: " ++ show t2 ++ " vs " ++ show t3)
    else throw ("condition of if must be Bool, got " ++ show t1)

  Zero -> return TNat
  Succ e -> checker e >>= \t -> if t == TNat then return TNat else throw ("succ expects Nat, got " ++ show t)
  Pred e -> checker e >>= \t -> if t == TNat then return TNat else throw ("pred expects Nat, got " ++ show t)
  IsZero e -> checker e >>= \t -> if t == TNat then return TBool else throw ("isZero expects Nat, got " ++ show t)
