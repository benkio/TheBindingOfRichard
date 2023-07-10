module Main (main) where

import Game.GameStateSpec (gameStateSpec)
import Game.Physics.CollisionDetectionSpec (collisionDetectionSpec)
import Model.EventSpec (gameEventSpec)
import Model.MoveSpec (moveSpec)
import Test.HUnit

-- Run the tests
main :: IO ()
main = do
    cs <-
        runTestTT $
            TestList
                [ moveSpec
                , gameEventSpec
                , gameStateSpec
                , collisionDetectionSpec
                ]
    print cs
