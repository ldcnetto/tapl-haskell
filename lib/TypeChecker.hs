module TypeChecker where

import AST
import Prelude hiding (fail)

type MErr = Either String Type

fail :: String -> MErr
fail = Left

checker :: Expr -> MErr
checker expr = case expr of
  ETrue -> return TBool
  EFalse -> return TBool
  If c t e -> do
    tc <- checker c
    tt <- checker t
    te <- checker e
    if tc == TBool
      then if tt == te then return tt else fail "both then and else clauses must have the same type"
      else fail "expecting boolean type in condition."
  Zero -> return TNat
  Succ e -> do
    te <- checker e
    if te == TNat then return TNat else fail "Succ expects a TNat"
  Pred e -> do
    te <- checker e
    if te == TNat then return TNat else fail "Pred expects a TNat"
  IsZero e -> do
    te <- checker e
    if te == TNat then return TBool else fail "IsZero expects a TNat"
