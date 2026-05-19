module AST where

type Name = String

-- Adicionamos EString, EUnit, Let e Ascribe
data Expr = ETrue
          | EFalse
          | If {cond :: Expr, exprThen :: Expr, exprElse :: Expr}
          | Zero
          | Succ Expr
          | Pred Expr
          | IsZero Expr
          | Var Name                  
          | Abs (Name, Type) Expr     
          | App Expr Expr             
          | EString String            -- NOVO: Base Type String
          | EUnit                     -- NOVO: Unit
          | Let Name Expr Expr        -- NOVO: Let Binding
          | Ascribe Expr Type         -- NOVO: Ascription (Anotação explícita t as T)
          | Add Expr Expr
          | Var Name                  -- x               vars in Lambda Calculus
          | Abs (Name, Type) Expr     -- (\x:T . expr)   abstraction in Lambda Calculus
          | App Expr Expr             -- t1 t2           application in Lambda Calculus
          | Pair Expr Expr            -- {t1, t2}  
          | Fst Expr                  -- t.1      
          | Snd Expr                  -- t.2       
          | Record [(Name, Expr)]     -- {l1=t1, ...} 
          | Proj Expr Name            -- t.l       
          | TmInl Expr Type           -- inl t as T
          | TmInr Expr Type           -- inr t as T
          | TmCase Expr (Name, Expr) (Name, Expr)        -- case t of inl x -> t1 | inr y -> t2
          | TmFix Expr                -- fix t
          | TNil Type                 -- nil[T]
          | TCons Type Expr Expr      -- cons[T] t1 t2
          | TIsNil Type Expr          -- isnil[T] t
          | THead Type Expr           -- head[T] t
          | TTail Type Expr           -- tail[T] t
     deriving (Eq, Show)

-- Atualizamos os Valores para incluir String e Unit
data Value = VTrue
           | VFalse
           | VZero
           | VSucc Value
           | VAbs (Name, Type) Expr
           | VString String           -- NOVO: Valor String
           | VUnit                    -- NOVO: Valor Unit
           | VPair Value Value
           | VRecord [(Name, Value)]
           | VNil Type                 -- nil[T] (valor)
           | VCons Type Value Value    -- cons[T] v1 v2 (valor)
     deriving (Eq, Show)

-- Adicionamos os novos tipos
data Type = TBool
          | TNat
          | Type `TArrow` Type
          | TString                   -- NOVO: Tipo Base String
          | TUnit                     -- NOVO: Tipo Unit
          | TProd Type Type           -- T1 x T2 (produto)
          | TRecord [(Name, Type)]    -- {l1:T1, ...}
          | TSum Type Type            -- T1 + T2 (soma)
          | TVariant [(Name, Type)]   -- <l1:T1, l2:T2, ...>
          | TUnit                     -- Unit type
          | TString                   -- String type
          | TFloat                    -- Float type
          | TList Type                -- List T
     deriving (Eq, Show)


-- A árvore sintática ganha um nó que guarda 
-- a expressão e o tipo reivindicado pelo programador.

data Expr = ...
          | Ascribe Expr Type