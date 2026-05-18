module TypeChecker where

import AST

import Control.Monad.State
import Control.Monad.Except (throwError)

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
--
-- In the symply typed lambda calculos, computations might not
-- only fail, but also manipulate a state. In our case, the
-- state is the type environment (or type context); a sequence
-- of tuples (Name, Type).
--
-- Since the type checker deals with two kinds of side effects
-- (errors and state), we can use the monad transformer StateT
-- to combine both the State and Either monads.
--
-- The state monad has the operations 'get' (to get the environment) and
-- 'put' (to update the environment).

type Env = [(Name, Type)]
type Err = Either String

type Res a = StateT Env Err a

checker :: Expr -> Res Type
checker expr = case expr of
  ETrue -> return TBool
  EFalse -> return TBool

  If e1 e2 e3 ->
    checker e1 >>= \t1 ->
    checker e2 >>= \t2 ->
    checker e3 >>= \t3 ->
    if t1 == TBool
    then if t2 == t3 then return t2 else throwError ("then/else branches have different types: " ++ show t2 ++ " vs " ++ show t3)
    else throwError ("condition of if must be Bool, got " ++ show t1)

  Zero -> return TNat
  Succ e -> checker e >>= \t -> if t == TNat then return TNat else throwError ("succ expects Nat, got " ++ show t)
  Pred e -> checker e >>= \t -> if t == TNat then return TNat else throwError ("pred expects Nat, got " ++ show t)
  IsZero e -> checker e >>= \t -> if t == TNat then return TBool else throwError ("isZero expects Nat, got " ++ show t)

  Var x -> do
    env <- get
    case lookup x env of
      Nothing -> throwError ("variable not in scope: " ++ x)
      Just t -> return t

  Abs (x, t1) e -> do
    env <- get             -- obtains the environment from the state
    put $ (x, t1) : env      -- updates the state with a new environment
    t2 <- checker e        -- checker for 'e' in the new environment
    put env                -- restores the environment
    return $ t1 `TArrow` t2

  App e1 e2 -> do
    t1 <- checker e1
    t2 <- checker e2

    case t1 of
      (t11 `TArrow` t12) -> if t2 == t11 then return t12 else throwError ("argument type mismatch: expected " ++ show t11 ++ ", got " ++ show t2)
      _ -> throwError ("expected a function type, got " ++ show t1)

  -- Regra T-Add: Soma de dois números naturais
  Add e1 e2 -> do
    t1 <- checker e1
    t2 <- checker e2
    if t1 == TNat && t2 == TNat
        then return TNat
        else throwError ("add expects both arguments to be Nat, got " ++ show t1 ++ " and " ++ show t2)

-- Regra T-Inl (usando TSum)
checker (TmInl t tySum) = do
    case tySum of
        TSum t1 t2 -> do
            tActual <- checker t
            if tActual == t1
                then return tySum
                else throwError ("inl: expected type " ++ show t1 ++ ", got " ++ show tActual)
        _ -> throwError ("inl: annotation must be a sum type (TSum), got " ++ show tySum)

-- Regra T-Inr (usando TSum)
checker (TmInr t tySum) = do
    case tySum of
        TSum t1 t2 -> do
            tActual <- checker t
            if tActual == t2
                then return tySum
                else throwError ("inr: expected type " ++ show t2 ++ ", got " ++ show tActual)
        _ -> throwError ("inr: annotation must be a sum type (TSum), got " ++ show tySum)

-- Regra T-Case (usando TSum)
checker (TmCase t0 (x1, t1) (x2, t2)) = do
    t0Type <- checker t0
    case t0Type of
        TSum ty1 ty2 -> do
            env <- get
            
            -- Primeiro braço (inl): assume x1 tem tipo ty1
            put ((x1, ty1) : env)
            t1Type <- checker t1
            
            -- Segundo braço (inr): assume x2 tem tipo ty2
            put ((x2, ty2) : env)
            t2Type <- checker t2
            
            -- Restaura o ambiente original
            put env
            
            -- Verifica se os dois braços retornam o mesmo tipo
            if t1Type == t2Type
                then return t1Type
                else throwError ("case branches have different types: " ++ show t1Type ++ " vs " ++ show t2Type)
        _ -> throwError ("case expected sum type (TSum), got " ++ show t0Type)

-- Regra T-Fix
checker (TmFix t) = do
    tType <- checker t
    case tType of
        TArrow t1 t2 -> 
            if t1 == t2
                then return t2
                else throwError ("fix: expected T->T, got " ++ show tType)
        _ -> throwError ("fix: expected function type (T->T), got " ++ show tType)

-- Regra T-Nil
checker (TNil ty) = do
    return (TList ty)

-- Regra T-Cons
checker (TCons ty t1 t2) = do
    t1Type <- checker t1
    t2Type <- checker t2
    if t1Type == ty
        then case t2Type of
            TList ty2 -> if ty == ty2
                then return (TList ty)
                else throwError ("cons: list element type mismatch")
            _ -> throwError ("cons: second argument must be List, got " ++ show t2Type)
        else throwError ("cons: first argument expected " ++ show ty ++ ", got " ++ show t1Type)

-- Regra T-IsNil
checker (TIsNil ty t) = do
    tType <- checker t
    case tType of
        TList ty' -> if ty == ty'
            then return TBool
            else throwError ("isnil: type mismatch")
        _ -> throwError ("isnil: expected List, got " ++ show tType)

-- Regra T-Head
checker (THead ty t) = do
    tType <- checker t
    case tType of
        TList ty' -> if ty == ty'
            then return ty
            else throwError ("head: type mismatch")
        _ -> throwError ("head: expected List, got " ++ show tType)

-- Regra T-Tail
checker (TTail ty t) = do
    tType <- checker t
    case tType of
        TList ty' -> if ty == ty'
            then return (TList ty)
            else throwError ("tail: type mismatch")
        _ -> throwError ("tail: expected List, got " ++ show tType)