module Main (main) where

import Test.HUnit
import Control.Monad.State (evalStateT)
import AST
import TypeChecker

run :: Expr -> Either String Type
run e = evalStateT (checker e) []

-- Bool literals
testTrue :: Test
testTrue = TestCase $ assertEqual "ETrue has type TBool" (Right TBool) (run ETrue)

testFalse :: Test
testFalse = TestCase $ assertEqual "EFalse has type TBool" (Right TBool) (run EFalse)

-- Nat literals
testZero :: Test
testZero = TestCase $ assertEqual "Zero has type TNat" (Right TNat) (run Zero)

testSuccZero :: Test
testSuccZero = TestCase $ assertEqual "Succ Zero has type TNat" (Right TNat) (run (Succ Zero))

testPredSuccZero :: Test
testPredSuccZero = TestCase $ assertEqual "Pred (Succ Zero) has type TNat" (Right TNat) (run (Pred (Succ Zero)))

testSuccNested :: Test
testSuccNested = TestCase $ assertEqual "Succ (Succ Zero) has type TNat" (Right TNat) (run (Succ (Succ Zero)))

-- IsZero
testIsZeroZero :: Test
testIsZeroZero = TestCase $ assertEqual "IsZero Zero has type TBool" (Right TBool) (run (IsZero Zero))

testIsZeroSucc :: Test
testIsZeroSucc = TestCase $ assertEqual "IsZero (Succ Zero) has type TBool" (Right TBool) (run (IsZero (Succ Zero)))

-- If expressions (well-typed)
testIfBoolBranches :: Test
testIfBoolBranches = TestCase $
  assertEqual "if true then true else false : TBool"
    (Right TBool)
    (run (If ETrue ETrue EFalse))

testIfNatBranches :: Test
testIfNatBranches = TestCase $
  assertEqual "if false then 0 else succ 0 : TNat"
    (Right TNat)
    (run (If EFalse Zero (Succ Zero)))

testIfCondIsZero :: Test
testIfCondIsZero = TestCase $
  assertEqual "if iszero 0 then 0 else succ 0 : TNat"
    (Right TNat)
    (run (If (IsZero Zero) Zero (Succ Zero)))

-- If expressions (ill-typed)
testIfNonBoolCond :: Test
testIfNonBoolCond = TestCase $
  case run (If Zero ETrue EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testIfBranchMismatch :: Test
testIfBranchMismatch = TestCase $
  case run (If ETrue Zero EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Succ / Pred on non-Nat (ill-typed)
testSuccBool :: Test
testSuccBool = TestCase $
  case run (Succ ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testPredBool :: Test
testPredBool = TestCase $
  case run (Pred EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- IsZero on non-Nat (ill-typed)
testIsZeroBool :: Test
testIsZeroBool = TestCase $
  case run (IsZero ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Var
testVarUnbound :: Test
testVarUnbound = TestCase $
  case run (Var "x") of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Abs (well-typed)
testAbsIdentityBool :: Test
testAbsIdentityBool = TestCase $
  assertEqual "\\x:Bool. x : Bool -> Bool"
    (Right (TBool `TArrow` TBool))
    (run (Abs ("x", TBool) (Var "x")))

testAbsIdentityNat :: Test
testAbsIdentityNat = TestCase $
  assertEqual "\\x:Nat. x : Nat -> Nat"
    (Right (TNat `TArrow` TNat))
    (run (Abs ("x", TNat) (Var "x")))

testAbsConstant :: Test
testAbsConstant = TestCase $
  assertEqual "\\x:Bool. zero : Bool -> Nat"
    (Right (TBool `TArrow` TNat))
    (run (Abs ("x", TBool) Zero))

testAbsNested :: Test
testAbsNested = TestCase $
  assertEqual "\\x:Bool. \\y:Nat. x : Bool -> Nat -> Bool"
    (Right (TBool `TArrow` (TNat `TArrow` TBool)))
    (run (Abs ("x", TBool) (Abs ("y", TNat) (Var "x"))))

-- App (well-typed)
testAppIdentityBool :: Test
testAppIdentityBool = TestCase $
  assertEqual "(\\x:Bool. x) true : TBool"
    (Right TBool)
    (run (App (Abs ("x", TBool) (Var "x")) ETrue))

testAppIdentityNat :: Test
testAppIdentityNat = TestCase $
  assertEqual "(\\x:Nat. x) zero : TNat"
    (Right TNat)
    (run (App (Abs ("x", TNat) (Var "x")) Zero))

testAppReturnsBool :: Test
testAppReturnsBool = TestCase $
  assertEqual "(\\x:Nat. isZero x) zero : TBool"
    (Right TBool)
    (run (App (Abs ("x", TNat) (IsZero (Var "x"))) Zero))

-- App (ill-typed)
testAppNotAFunction :: Test
testAppNotAFunction = TestCase $
  case run (App ETrue EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testAppArgMismatch :: Test
testAppArgMismatch = TestCase $
  case run (App (Abs ("x", TBool) (Var "x")) Zero) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Teste: inl de um valor em uma soma
testInl :: Test
testInl = TestCase $
    let pa = Record [("firstlast", ETrue), ("addr", ETrue)]
        leftType = TRecord [("firstlast", TBool), ("addr", TBool)]
        rightType = TRecord [("name", TBool), ("email", TBool)]
        addrTy = TSum leftType rightType   
        expr = TmInl pa addrTy
    in assertEqual "inl pa as Addr : Addr"
        (Right addrTy)
        (run expr)

-- Teste: inr de um valor em uma soma
testInr :: Test
testInr = TestCase $
    let va = Record [("name", ETrue), ("email", ETrue)]
        leftType = TRecord [("firstlast", TBool), ("addr", TBool)]
        rightType = TRecord [("name", TBool), ("email", TBool)]
        addrTy = TSum leftType rightType   
        expr = TmInr va addrTy
    in assertEqual "inr va as Addr : Addr"
        (Right addrTy)
        (run expr)

-- Teste: case em uma soma (inl)
testCaseInl :: Test
testCaseInl = TestCase $
    let pa = Record [("firstlast", ETrue), ("addr", ETrue)]
        leftType = TRecord [("firstlast", TBool), ("addr", TBool)]
        rightType = TRecord [("name", TBool), ("email", TBool)]
        addrTy = TSum leftType rightType
        sumExpr = TmInl pa addrTy
        -- case a of inl x -> x.firstlast | inr y -> y.name
        caseExpr = TmCase sumExpr 
                    ("x", Proj (Var "x") "firstlast")
                    ("y", Proj (Var "y") "name")
    in assertEqual "case inl returns firstlast (Bool)"
        (Right TBool)
        (run caseExpr)

-- Teste: case em uma soma (inr)
testCaseInr :: Test
testCaseInr = TestCase $
    let va = Record [("name", ETrue), ("email", ETrue)]
        leftType = TRecord [("firstlast", TBool), ("addr", TBool)]
        rightType = TRecord [("name", TBool), ("email", TBool)]
        addrTy = TSum leftType rightType
        sumExpr = TmInr va addrTy
        caseExpr = TmCase sumExpr 
                    ("x", Proj (Var "x") "firstlast")
                    ("y", Proj (Var "y") "name")
    in assertEqual "case inr returns name (Bool)"
        (Right TBool)
        (run caseExpr)

-- Teste: case com branches de tipos diferentes (deve falhar)
testCaseBranchMismatch :: Test
testCaseBranchMismatch = TestCase $
    let leftType = TRecord []
        rightType = TRecord []
        sumType = TSum leftType rightType
        sumExpr = TmInl (Record []) sumType
        -- Primeiro braço retorna Zero (TNat), segundo retorna True (TBool)
        caseExpr = TmCase sumExpr 
                    ("x", Zero)
                    ("y", ETrue)
    in case run caseExpr of
        Left _  -> return ()
        Right t -> assertFailure ("expected type error, got " ++ show t)

-- Teste: case em uma expressão que não é soma (deve falhar)
testCaseNotSum :: Test
testCaseNotSum = TestCase $
    let notSumExpr = Zero  
        caseExpr = TmCase notSumExpr 
                    ("x", Zero)
                    ("y", Zero)
    in case run caseExpr of
        Left _  -> return ()
        Right t -> assertFailure ("expected type error, got " ++ show t)

-- Teste: fix em uma função simples
testFixSimple :: Test
testFixSimple = TestCase $
    -- fix (λx:Nat. x)  
    let func = Abs ("x", TNat) (Var "x")
        expr = TmFix func
    in assertEqual "fix (λx:Nat.x) : Nat"
        (Right TNat)
        (run expr)

-- Teste: fix em uma função que não é T->T (deve falhar)
testFixWrongType :: Test
testFixWrongType = TestCase $
    -- fix (λx:Nat. x) espera Nat, mas True não é função!
    -- Ou melhor: fix aplicado a um termo que não é função
    let expr = TmFix ETrue  -- fix true (isso sim é erro!)
    in case run expr of
        Left _  -> return ()
        Right t -> assertFailure ("expected type error, got " ++ show t)

-- Teste: fix aplicado a uma função que gera iseven (tipagem apenas)
testFixIseven :: Test
testFixIseven = TestCase $
    let ieType = TNat `TArrow` TBool
        inner = Abs ("n", TNat) 
                    (If (IsZero (Var "n"))
                        ETrue
                        (If (IsZero (Pred (Var "n")))
                            EFalse
                            (App (Var "ie") (Pred (Pred (Var "n"))))))
        generator = Abs ("ie", ieType) inner
        expr = TmFix generator
    in assertEqual "fix (gerador iseven) : Nat -> Bool"
        (Right (TNat `TArrow` TBool))
        (run expr)

-- Teste: nil[T] tem tipo List T
testNil :: Test
testNil = TestCase $
    let expr = TNil TNat
    in assertEqual "nil[Nat] : List Nat"
        (Right (TList TNat))
        (run expr)

-- Teste: cons de um elemento em uma lista vazia
testCons :: Test
testCons = TestCase $
    let expr = TCons TNat Zero (TNil TNat)
    in assertEqual "cons[Nat] zero nil : List Nat"
        (Right (TList TNat))
        (run expr)

-- Teste: cons com tipo incompatível (deve falhar)
testConsTypeMismatch :: Test
testConsTypeMismatch = TestCase $
    let expr = TCons TNat ETrue (TNil TNat)
    in case run expr of
        Left _  -> return ()
        Right t -> assertFailure ("expected type error, got " ++ show t)

-- Teste: isnil em lista vazia
testIsNilTrue :: Test
testIsNilTrue = TestCase $
    let expr = TIsNil TNat (TNil TNat)
    in assertEqual "isnil[Nat] nil : Bool"
        (Right TBool)
        (run expr)

-- Teste: isnil em lista não-vazia
testIsNilFalse :: Test
testIsNilFalse = TestCase $
    let expr = TIsNil TNat (TCons TNat Zero (TNil TNat))
    in assertEqual "isnil[Nat] (cons zero nil) : Bool"
        (Right TBool)
        (run expr)

-- Teste: head de uma lista não-vazia
testHead :: Test
testHead = TestCase $
    let expr = THead TNat (TCons TNat Zero (TNil TNat))
    in assertEqual "head[Nat] (cons zero nil) : Nat"
        (Right TNat)
        (run expr)

-- Teste: head de lista vazia (tipa, mas avaliação falharia)
testHeadNil :: Test
testHeadNil = TestCase $
    let expr = THead TNat (TNil TNat)
    in assertEqual "head[Nat] nil : Nat (tipa, mas runtime error)"
        (Right TNat)
        (run expr)

-- Teste: tail de uma lista não-vazia
testTail :: Test
testTail = TestCase $
    let expr = TTail TNat (TCons TNat Zero (TNil TNat))
    in assertEqual "tail[Nat] (cons zero nil) : List Nat"
        (Right (TList TNat))
        (run expr)

-- Teste: soma de lista (exemplo do slide) 
testSumList :: Test
testSumList = TestCase $
    let sumListGenerator = 
            Abs ("sum", TList TNat `TArrow` TNat)
                (Abs ("l", TList TNat)
                    (If (TIsNil TNat (Var "l"))
                        Zero
                        (Add (THead TNat (Var "l"))
                             (App (Var "sum") (TTail TNat (Var "l")))))) 
        sumList = TmFix sumListGenerator
        lista = TCons TNat (Succ (Succ Zero))  
                (TCons TNat (Succ Zero)       
                    (TNil TNat))
        expr = App sumList lista
    in assertEqual "sumList [2,1] : Nat"
        (Right TNat)
        (run expr)     

tests :: Test
tests = TestList
  [ TestLabel "ETrue"                testTrue
  , TestLabel "EFalse"               testFalse
  , TestLabel "Zero"                 testZero
  , TestLabel "Succ Zero"            testSuccZero
  , TestLabel "Pred (Succ Zero)"     testPredSuccZero
  , TestLabel "Succ (Succ Zero)"     testSuccNested
  , TestLabel "IsZero Zero"          testIsZeroZero
  , TestLabel "IsZero (Succ Zero)"   testIsZeroSucc
  , TestLabel "If bool branches"     testIfBoolBranches
  , TestLabel "If nat branches"      testIfNatBranches
  , TestLabel "If iszero cond"       testIfCondIsZero
  , TestLabel "If non-bool cond"     testIfNonBoolCond
  , TestLabel "If branch mismatch"   testIfBranchMismatch
  , TestLabel "Succ Bool"            testSuccBool
  , TestLabel "Pred Bool"            testPredBool
  , TestLabel "IsZero Bool"          testIsZeroBool
  , TestLabel "Var unbound"          testVarUnbound
  , TestLabel "Abs identity Bool"    testAbsIdentityBool
  , TestLabel "Abs identity Nat"     testAbsIdentityNat
  , TestLabel "Abs constant"         testAbsConstant
  , TestLabel "Abs nested"           testAbsNested
  , TestLabel "App identity Bool"    testAppIdentityBool
  , TestLabel "App identity Nat"     testAppIdentityNat
  , TestLabel "App returns Bool"     testAppReturnsBool
  , TestLabel "App not a function"   testAppNotAFunction
  , TestLabel "App arg mismatch"     testAppArgMismatch
  , TestLabel "TmInl"                testInl
  , TestLabel "TmInr"                testInr
  , TestLabel "TmCase inl"           testCaseInl
  , TestLabel "TmCase inr"           testCaseInr
  , TestLabel "TmCase branch mismatch" testCaseBranchMismatch
  , TestLabel "TmCase not sum"       testCaseNotSum
  , TestLabel "TmFix simple"         testFixSimple
  , TestLabel "TmFix wrong type"     testFixWrongType
  , TestLabel "TmFix iseven generator" testFixIseven
  , TestLabel "TNil"                 testNil
  , TestLabel "TCons"                testCons
  , TestLabel "TCons type mismatch"  testConsTypeMismatch
  , TestLabel "TIsNil true"          testIsNilTrue
  , TestLabel "TIsNil false"         testIsNilFalse
  , TestLabel "THead"                testHead
  , TestLabel "THead nil"            testHeadNil
  , TestLabel "TTail"                testTail
  , TestLabel "sumList example"      testSumList
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result + failures result > 0
    then fail "Some tests failed."
    else return ()