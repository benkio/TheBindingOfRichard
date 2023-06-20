module Main (main) where

import CollisionDetectionSpec (collisionDetectionSpec)
import GameEventSpec (gameEventSpec)
import GameSetup (GameSetup (..), withGameSetup)
import GameStateSpec (gameStateSpec)
import MoveSpec (moveSpec)
import Test.HUnit

-- Run the tests
main :: IO ()
main = withGameSetup $ \gameSetup -> do
    cs <-
        runTestTT $
            TestList
                [ moveSpec
                , gameEventSpec
                , gameStateSpec (gameSetup{windowSize = (100, 100)})
                , collisionDetectionSpec
                ]
    print cs
