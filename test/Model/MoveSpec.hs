{-# LANGUAGE LambdaCase #-}

module Model.MoveSpec (moveSpec) where

import Graphics.Point (Point (..))
import qualified Model.Move as M
import Test.HUnit

moveSpec :: Test
moveSpec =
    TestList
        [ TestLabel "movePoint should move a point in the expected position" testMovePoint
        ]

testMovePoint :: Test
testMovePoint =
    TestCase $
        let move = enumFrom M.Up
            point = Point{x = 0, y = 0}
            actual = fmap (`M.movePoint` point) move
            expected =
                fmap
                    ( \case
                        M.Up -> point{y = y point - M.stepSize}
                        M.Down -> point{y = y point + M.stepSize}
                        M.Left -> point{x = x point - M.stepSize}
                        M.Right -> point{x = x point + M.stepSize}
                    )
                    move
         in assertEqual "PointMoved to the right position" actual expected
