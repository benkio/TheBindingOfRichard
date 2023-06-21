module Main (main) where

import Game.GameEventSpec (gameEventSpec)
import Game.GameStateSpec (gameStateSpec)
import Game.Model.MoveSpec (moveSpec)
import Game.Physics.CollisionDetectionSpec (collisionDetectionSpec)
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
