module GameEventSpec (gameEventSpec) where

import Controls (defaultControls)
import Data.Foldable (traverse_)
import GameEvent (GameEvent (..), toGameEvent)
import qualified Move
import qualified SDL
import Test.HUnit

gameEventSpec :: Test
gameEventSpec =
    TestList
        [ TestLabel "toGameEvent: SDL.Event conversion" testToGameEvent
        ]

testToGameEvent :: Test
testToGameEvent =
    TestCase $
        traverse_
            ( \(event, expected) ->
                assertEqual
                    "toGameEvent: The conversion from SDL.Event to Game Event is successful"
                    (toGameEvent event defaultControls)
                    expected
            )
            eventMap

eventMap :: [(SDL.Event, GameEvent)]
eventMap =
    [ (buildKeypressEvent SDL.KeycodeUp, GameEvent.GE Move.Up)
    , (buildKeypressEvent SDL.KeycodeDown, GameEvent.GE Move.Down)
    , (buildKeypressEvent SDL.KeycodeLeft, GameEvent.GE Move.Left)
    , (buildKeypressEvent SDL.KeycodeRight, GameEvent.GE Move.Right)
    , (buildKeypressEvent SDL.KeycodeQ, GameEvent.Quit)
    ]

buildKeypressEvent :: SDL.Keycode -> SDL.Event
buildKeypressEvent keycode =
    SDL.Event
        { SDL.eventTimestamp = undefined
        , SDL.eventPayload =
            SDL.KeyboardEvent $
                SDL.KeyboardEventData
                    { SDL.keyboardEventWindow = Nothing
                    , SDL.keyboardEventKeyMotion = SDL.Pressed
                    , SDL.keyboardEventRepeat = False
                    , SDL.keyboardEventKeysym = SDL.Keysym undefined keycode undefined
                    }
        }
