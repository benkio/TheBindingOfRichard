module Model.EventSpec (gameEventSpec) where

import Data.Foldable (traverse_)
import Model.Event (toEventDefaultControl)
import Settings.Controls (defaultControls)
import Test.HUnit
import TestOps (eventMap)

gameEventSpec :: Test
gameEventSpec =
    TestList
        [ TestLabel "toEventDefaultControl: SDL.Event conversion" testToEvent
        ]

testToEvent :: Test
testToEvent =
    TestCase $
        traverse_
            ( \(event, expected, _) ->
                assertEqual
                    "toEventDefaultControl: The conversion from SDL.Event to Game Event is successful"
                    (toEventDefaultControl event defaultControls)
                    (Just expected)
            )
            eventMap
