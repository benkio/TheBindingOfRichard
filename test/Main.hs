module Main (main) where

import MoveSpec (moveSpec)
import Test.HUnit

-- Run the tests
main :: IO ()
main = do
    counts <- runTestTT moveSpec
    putStrLn $ show counts
