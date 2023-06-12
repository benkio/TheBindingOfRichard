module TestOps (eventMap, arrowEventMap, quitEventMap, testGameState, buildKeypressEvent) where

import GameEvent (GameEvent (..))
import GameState (GameState (..), positionL)
import Graphics.Point (Point (..))
import qualified Move
import qualified SDL
import Control.Lens

testGameState :: GameState
testGameState = GameState{position = Point{x = 50, y = 50}}

eventMap :: [(SDL.Event, GameEvent, GameState -> GameState)]
eventMap =
    arrowEventMap ++ quitEventMap

quitEventMap :: [(SDL.Event, GameEvent, GameState -> GameState)]
quitEventMap = [(buildKeypressEvent SDL.KeycodeQ, GameEvent.Quit, id)]

arrowEventMap :: [(SDL.Event, GameEvent, GameState -> GameState)]
arrowEventMap = [ (buildKeypressEvent SDL.KeycodeUp, GameEvent.GE Move.Up, set positionL (Point{x = 50, y = 50 - Move.stepSize}))
    , (buildKeypressEvent SDL.KeycodeDown, GameEvent.GE Move.Down        , set positionL (Point{x = 50, y = 50 + Move.stepSize}))
    , (buildKeypressEvent SDL.KeycodeLeft, GameEvent.GE Move.Left        , set positionL (Point{x = 50 - Move.stepSize, y = 50}))
    , (buildKeypressEvent SDL.KeycodeRight, GameEvent.GE Move.Right      , set positionL (Point{x = 50 + Move.stepSize, y = 50}))]

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
