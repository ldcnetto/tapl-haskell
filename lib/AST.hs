module AST where

type Name = String

data Expr = ETrue
          | EFalse
          | If {cond :: Expr, exprThen :: Expr, exprElse :: Expr}
          | Zero
          | Succ Expr
          | Pred Expr
          | IsZero Expr
          | Var Name                  -- x               vars in Lambda Calculus
          | Abs (Name, Type) Expr     -- (\x:T . expr)   abstraction in Lambda Calculus
          | App Expr Expr             -- t1 t2           application in Lambda Calculus
          | Pair Expr Expr            -- {t1, t2}        pair creation
          | Fst Expr                  -- t.1             first projection
          | Snd Expr                  -- t.2             second projection
          | Record [(Name, Expr)]     -- {l1=t1, ...}    record creation
          | Proj Expr Name            -- t.l             record projection
     deriving (Eq, Show)

data Value = VTrue
           | VFalse
           | VZero
           | VSucc Value
           | VAbs (Name, Type) Expr
           | VPair Value Value
           | VRecord [(Name, Value)]
     deriving (Eq, Show)

data Type = TBool
          | TNat
          | Type `TArrow` Type
          | TProd Type Type           -- T1 x T2         product type
          | TRecord [(Name, Type)]    -- {l1:T1, ...}    record type
     deriving (Eq, Show)
