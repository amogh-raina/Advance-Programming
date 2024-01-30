-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the Droll APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.

import Types
import Parser
import Calculator

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (isInfixOf)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

-- VERY rudimentary for now
tests = testGroup "Smoke tests" [
  --Parser tests
  testCase "parseExp" $
    parseExp "roll (2+2)" @?= Right expa,

  testCase "parseExp: Valid let expression" $
    parseExp "let x be 5 in x" @?= Right (Let "x" (Cst 5) (Just (Var "x"))),

  testCase "parseExp: Valid addition expression" $
    parseExp "2 + 3" @?= Right (Sum (Join [Cst 2, Cst 3])),
  
  testCase "parseExp: Valid variable expression" $
    parseExp "x" @?= Right (Var "x"),

  testCase "parseExp: Valid times expression" $
    parseExp "2 times 3" @?= Right (Times (Cst 2) (Cst 3)),
  
  testCase "parseExp: Valid comparison expression" $
    parseExp "2 is not 3" @?= Right (Is (Cst 2) (Cst 3)),

  testCase "parseExp: Valid take expression" $
    parseExp "take highest from x" @?= Right (Take Max "x"),

  testCase "parseExp: Valid count expression" $
    parseExp "count 5" @?= Right (Count (Cst 5)),

  testCase "parseExp: Valid sum expression" $
    parseExp "sum 2+3" @?= Right (Sum (Join [Cst 2, Cst 3])),

  testCase "parseExp: Valid roll expression with spaces" $
    parseExp "roll (2 + 2)" @?= Right (Roll (Sum (Join [Cst 2, Cst 2]))),

  testCase "parseExp: Invalid expression with unbalanced parentheses" $ do
      let result = parseExp "((2 + 2)"
      case result of
          Left _ -> pure ()  -- Expected error, do nothing
          Right _ -> assertFailure "Should have failed due to unbalanced parentheses",

  testCase "parseExp: Valid expression with trailing characters" $
    case parseExp "2 + 2 extra" of
        Left _ -> assertBool "Correctly identified extra characters" True
        Right _ -> assertFailure "Should have failed due to extra characters",

  testCase "parseExp: Expression with invalid operator" $
    case parseExp "2 ^ 2" of
        Left _ -> assertBool "Correctly identified invalid operator" True
        Right _ -> assertFailure "Should have failed due to invalid operator",


  -- Evaluation tests
  testCase "evaluate" $
    runPD (evaluate expa) @?= [(1/4, Right (Bag [i])) | i <- [1..4]],
  
  testCase "evaluate constant" $
      runPD (evaluate (Cst 5)) @?= [(1, Right (Bag [5]))],
  
  testCase "evaluate sum of constants" $
      runPD (evaluate (Sum (Join [Cst 3, Cst 4]))) @?= [(1, Right (Bag [7]))],

  testCase "evaluate variable lookup" $ do
      let store = [("x", Bag [5])]
      runPD (runCalc (eval (Var "x")) store) @?= [(1, Right (Bag [5], store))],

  -- Error handling tests
  testCase "evaluate unbound variable" $ do
    let store = []
    runPD (runCalc (eval (Var "x")) store) @?= [(1, Left (Unbound "x" "Variable not found"))],

  testCase "roll with non-positive sides" $
    runPD (evaluate (Roll (Cst 0))) @?= [(1, Left (OtherError "Cannot roll a die with non-positive sides"))],
  
  -- Complex expression tests
  testCase "roll a die of sum of constants" $
    runPD (evaluate expa) @?= [(1/4, Right (Bag [i])) | i <- [1..4]],

  testCase "evaluate let expression" $
    let expb = Let "x" (Cst 5) (Just (Var "x"))
    in runPD (evaluate expb) @?= [(1, Right (Bag [5]))],

  testCase "evaluate sequence of expressions" $
    let expc = Seq (Cst 3) (Cst 4)
    in runPD (evaluate expc) @?= [(1, Right (Bag [4]))]
  ]
  where
    expa = Roll (Sum (Join [Cst 2, Cst 2]))

