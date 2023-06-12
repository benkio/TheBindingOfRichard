module MoveSpec (moveSpec) where

import Graphics.Point (Point (..))
import qualified Move
import Test.HUnit

moveSpec :: Test
moveSpec =
    TestList
        [ TestLabel "movePoint should move a point in the expected position" testMovePoint
        ]

testMovePoint :: Test
testMovePoint =
    TestCase $
        let move = enumFrom Move.Up
            point = Point{x = 0, y = 0}
            actual = fmap (\m -> Move.movePoint m point) move
            expected =
                fmap
                    ( \m -> case m of
                        Move.Up -> point{y = y point - Move.stepSize}
                        Move.Down -> point{y = y point + Move.stepSize}
                        Move.Left -> point{x = x point - Move.stepSize}
                        Move.Right -> point{x = x point + Move.stepSize}
                        Move.Rest -> point
                    )
                    move
         in assertEqual "PointMoved to the right position" actual expected
