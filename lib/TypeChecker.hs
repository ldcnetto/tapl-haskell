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
.
  -- NOVO: Regra para Tipos Base (String)
  -- Uma string literal sempre tem o tipo TString
  EString _ -> return TString

  -- NOVO: Regra para Unit
  -- O termo unit sempre tem o tipo TUnit
  EUnit -> return TUnit

  -- NOVO: Regra para Ascription (t as T)
  -- O compilador avalia o tipo de 'e'. Se bater com o tipo 't' anotado pelo programador, passa!
  Ascribe e tAnotado -> do
    tCalculado <- checker e
    if tCalculado == tAnotado 
      then return tAnotado 
      else throwError ("Erro de Ascription: o termo tem tipo " ++ show tCalculado ++ " mas foi anotado como " ++ show tAnotado)

  -- NOVO: Regra para Let Binding (let x = e1 in e2)
  -- Exatamente o código do seu Slide 17!
  Let x e1 e2 -> do
    t1 <- checker e1       -- 1. Descobre o tipo da variável que estamos criando
    env <- get             -- 2. Pega o caderno de anotações (ambiente) atual
    put $ (x, t1) : env    -- 3. Anota a nova variável 'x' e seu tipo no caderno
    t2 <- checker e2       -- 4. Avalia o resto do código (e2) com o 'x' existindo lá
    put env                -- 5. Apaga o 'x' restaurando o caderno original
    return t2              -- 6. Retorna o tipo final do bloco let
