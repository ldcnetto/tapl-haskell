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
     deriving (Eq, Show)

-- Atualizamos os Valores para incluir String e Unit
data Value = VTrue
           | VFalse
           | VZero
           | VSucc Value
           | VAbs (Name, Type) Expr
           | VString String           -- NOVO: Valor String
           | VUnit                    -- NOVO: Valor Unit
     deriving (Eq, Show)

-- Adicionamos os novos tipos
data Type = TBool
          | TNat
          | Type `TArrow` Type
          | TString                   -- NOVO: Tipo Base String
          | TUnit                     -- NOVO: Tipo Unit
     deriving (Eq, Show)