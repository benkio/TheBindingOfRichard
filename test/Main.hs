module Main (main) where

import Test.HUnit

-- Function to test
addTwoNumbers :: Int -> Int -> Int
addTwoNumbers x y = x + y

-- Test case
testAddTwoNumbers :: Test
testAddTwoNumbers =
    TestCase $ do
        let result = addTwoNumbers 2 3
        assertEqual "Addition failed" 5 result

-- Test suite
testSuite :: Test
testSuite =
    TestList
        [ TestLabel "Test addition" testAddTwoNumbers
        ]

-- Run the tests
main :: IO ()
main = do
    counts <- runTestTT testSuite
    putStrLn $ show counts
