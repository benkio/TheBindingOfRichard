module Game.GameEventSpec (gameEventSpec) where

import Data.Foldable (traverse_)
import Game.GameEvent (toGameEvent)
import Settings.Controls (defaultControls)
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
