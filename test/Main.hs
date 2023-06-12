module Main (main) where

import GameEventSpec (gameEventSpec)
import MoveSpec (moveSpec)
import Test.HUnit

-- Run the tests
main :: IO ()
main = do
    counts <- runTestTT $ TestList [moveSpec, gameEventSpec]
    putStrLn $ show counts
