module GameEventSpec (gameEventSpec) where

import Controls (defaultControls)
import Data.Foldable (traverse_)
import GameEvent (toGameEvent)
import Test.HUnit
import TestOps (eventMap)

gameEventSpec :: Test
gameEventSpec =
    TestList
        [ TestLabel "toGameEvent: SDL.Event conversion" testToGameEvent
        ]

testToGameEvent :: Test
testToGameEvent =
    TestCase $
        traverse_
            ( \(event, expected, _) ->
                assertEqual
                    "toGameEvent: The conversion from SDL.Event to Game Event is successful"
                    (toGameEvent event defaultControls)
                    expected
            )
            eventMap
