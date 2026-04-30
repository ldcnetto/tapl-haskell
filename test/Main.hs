module Main (main) where

import Test.HUnit
import AST
import TypeChecker

-- Bool literals
testTrue :: Test
testTrue = TestCase $ assertEqual "ETrue has type TBool" (Right TBool) (checker ETrue)

testFalse :: Test
testFalse = TestCase $ assertEqual "EFalse has type TBool" (Right TBool) (checker EFalse)

-- Nat literals
testZero :: Test
testZero = TestCase $ assertEqual "Zero has type TNat" (Right TNat) (checker Zero)

testSuccZero :: Test
testSuccZero = TestCase $ assertEqual "Succ Zero has type TNat" (Right TNat) (checker (Succ Zero))

testPredSuccZero :: Test
testPredSuccZero = TestCase $ assertEqual "Pred (Succ Zero) has type TNat" (Right TNat) (checker (Pred (Succ Zero)))

testSuccNested :: Test
testSuccNested = TestCase $ assertEqual "Succ (Succ Zero) has type TNat" (Right TNat) (checker (Succ (Succ Zero)))

-- IsZero
testIsZeroZero :: Test
testIsZeroZero = TestCase $ assertEqual "IsZero Zero has type TBool" (Right TBool) (checker (IsZero Zero))

testIsZeroSucc :: Test
testIsZeroSucc = TestCase $ assertEqual "IsZero (Succ Zero) has type TBool" (Right TBool) (checker (IsZero (Succ Zero)))

-- If expressions (well-typed)
testIfBoolBranches :: Test
testIfBoolBranches = TestCase $
  assertEqual "if true then true else false : TBool"
    (Right TBool)
    (checker (If ETrue ETrue EFalse))

testIfNatBranches :: Test
testIfNatBranches = TestCase $
  assertEqual "if false then 0 else succ 0 : TNat"
    (Right TNat)
    (checker (If EFalse Zero (Succ Zero)))

testIfCondIsZero :: Test
testIfCondIsZero = TestCase $
  assertEqual "if iszero 0 then 0 else succ 0 : TNat"
    (Right TNat)
    (checker (If (IsZero Zero) Zero (Succ Zero)))

-- If expressions (ill-typed)
testIfNonBoolCond :: Test
testIfNonBoolCond = TestCase $
  case checker (If Zero ETrue EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testIfBranchMismatch :: Test
testIfBranchMismatch = TestCase $
  case checker (If ETrue Zero EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Succ / Pred on non-Nat (ill-typed)
testSuccBool :: Test
testSuccBool = TestCase $
  case checker (Succ ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testPredBool :: Test
testPredBool = TestCase $
  case checker (Pred EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- IsZero on non-Nat (ill-typed)
testIsZeroBool :: Test
testIsZeroBool = TestCase $
  case checker (IsZero ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

tests :: Test
tests = TestList
  [ TestLabel "ETrue"             testTrue
  , TestLabel "EFalse"            testFalse
  , TestLabel "Zero"              testZero
  , TestLabel "Succ Zero"         testSuccZero
  , TestLabel "Pred (Succ Zero)"  testPredSuccZero
  , TestLabel "Succ (Succ Zero)"  testSuccNested
  , TestLabel "IsZero Zero"       testIsZeroZero
  , TestLabel "IsZero (Succ Zero)"testIsZeroSucc
  , TestLabel "If bool branches"  testIfBoolBranches
  , TestLabel "If nat branches"   testIfNatBranches
  , TestLabel "If iszero cond"    testIfCondIsZero
  , TestLabel "If non-bool cond"  testIfNonBoolCond
  , TestLabel "If branch mismatch"testIfBranchMismatch
  , TestLabel "Succ Bool"         testSuccBool
  , TestLabel "Pred Bool"         testPredBool
  , TestLabel "IsZero Bool"       testIsZeroBool
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result + failures result > 0
    then fail "Some tests failed."
    else return ()
