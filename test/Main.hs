module Main (main) where

import Test.HUnit
import MoveSpec (moveSpec)

-- Run the tests
main :: IO ()
main = do
    counts <- runTestTT moveSpec
    putStrLn $ show counts
