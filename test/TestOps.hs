module TestOps (eventMap, arrowEventMap, quitEventMap, testGameState, buildKeypressEvent) where

import Player (Player (..), playerPositionL)

import Control.Lens
import GameEvent (GameEvent (..))
import GameState (GameState (..), gameStatePlayerL)
import Graphics.Point (Point (..))
import qualified Move
import qualified SDL

testGameState :: GameState
testGameState = GameState{player = Player{position = Point{x = 50, y = 50}}}

eventMap :: [(SDL.Event, GameEvent, GameState -> GameState)]
eventMap =
    arrowEventMap ++ quitEventMap

quitEventMap :: [(SDL.Event, GameEvent, GameState -> GameState)]
quitEventMap = [(buildKeypressEvent SDL.KeycodeQ, GameEvent.Quit, id)]

arrowEventMap :: [(SDL.Event, GameEvent, GameState -> GameState)]
arrowEventMap =
    [ (buildKeypressEvent SDL.KeycodeUp, GameEvent.GE Move.Up, set (gameStatePlayerL . playerPositionL) (Point{x = 50, y = 50 - Move.stepSize}))
    , (buildKeypressEvent SDL.KeycodeDown, GameEvent.GE Move.Down, set (gameStatePlayerL . playerPositionL) (Point{x = 50, y = 50 + Move.stepSize}))
    , (buildKeypressEvent SDL.KeycodeLeft, GameEvent.GE Move.Left, set (gameStatePlayerL . playerPositionL) (Point{x = 50 - Move.stepSize, y = 50}))
    , (buildKeypressEvent SDL.KeycodeRight, GameEvent.GE Move.Right, set (gameStatePlayerL . playerPositionL) (Point{x = 50 + Move.stepSize, y = 50}))
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
