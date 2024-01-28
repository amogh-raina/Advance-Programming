-- Sample black-box test suite. Feel free to adapt, or start from scratch.

-- Do NOT import from your ModImpl files here. These tests should work with
-- any implementation of the Droll APIs. Put any white-box tests in
-- suite1/WhiteBox.hs.

import Types
import Parser
import Calculator

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

-- VERY rudimentary for now
tests = testGroup "Smoke tests" [
  testCase "parseExp" $
    parseExp "roll (2+2)" @?= Right expa,
  testCase "evaluate" $
    runPD (evaluate expa) @?= [(1/4, Right (Bag [i])) | i <- [1..4]]
  ]
  where
    expa = Roll (Sum (Join [Cst 2, Cst 2]))

