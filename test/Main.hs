module Main (main) where

import GameEventSpec (gameEventSpec)
import GameStateSpec (gameStateSpec)
import MoveSpec (moveSpec)
import CollisionDetectionSpec (collisionDetectionSpec)
import Test.HUnit

-- Run the tests
main :: IO ()
main = do
    counts <- runTestTT $ TestList [moveSpec, gameEventSpec, gameStateSpec, collisionDetectionSpec]
    print counts
