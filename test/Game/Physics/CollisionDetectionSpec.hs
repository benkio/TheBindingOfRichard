module Game.Physics.CollisionDetectionSpec (collisionDetectionSpec) where

import Game.Model.Room (standardRoom)
import Game.Physics.CollisionDetection (isWithinRoom)
import Graphics.Point (Point (..))
import Test.HUnit

collisionDetectionSpec :: Test
collisionDetectionSpec =
    TestList
        [ TestLabel "isWithinRoom: should return true if the point is in the room" testIsWithinRoomInner
        , TestLabel "isWithinRoom: should return false if the point is in the room" testIsWithinRoomOuter
        ]

testIsWithinRoomInner :: Test
testIsWithinRoomInner =
    TestCase $
        assertBool "isWithinRoom return true because the point is in the room" (isWithinRoom (Point{x = 50, y = 50}) (standardRoom 100 100))

testIsWithinRoomOuter :: Test
testIsWithinRoomOuter =
    TestCase $
        assertBool "isWithinRoom return false because the point is not in the room" $
            not (isWithinRoom (Point{x = 0, y = 0}) (standardRoom 100 100))
