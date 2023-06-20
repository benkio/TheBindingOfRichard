module Main (main) where

import CollisionDetectionSpec (collisionDetectionSpec)
import GameEventSpec (gameEventSpec)
import GameStateSpec (gameStateSpec)
import MoveSpec (moveSpec)
import Test.HUnit

-- Run the tests
main :: IO ()
main = do
    counts <- runTestTT $ TestList [moveSpec, gameEventSpec, gameStateSpec, collisionDetectionSpec]
    print counts
