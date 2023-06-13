module TestOps (eventMap, arrowEventMap, quitEventMap, testGameState, buildKeypressEvent) where

import Model.Player (Player (..), playerPositionL)

import Control.Lens
import GameEvent (GameEvent (..))
import GameState (GameState (..), gameStatePlayerL)
import Graphics.Point (Point (..))
import qualified Model.Move as M
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
    [ (buildKeypressEvent SDL.KeycodeUp, GameEvent.GE M.Up, set (gameStatePlayerL . playerPositionL) (Point{x = 50, y = 50 - M.stepSize}))
    , (buildKeypressEvent SDL.KeycodeDown, GameEvent.GE M.Down, set (gameStatePlayerL . playerPositionL) (Point{x = 50, y = 50 + M.stepSize}))
    , (buildKeypressEvent SDL.KeycodeLeft, GameEvent.GE M.Left, set (gameStatePlayerL . playerPositionL) (Point{x = 50 - M.stepSize, y = 50}))
    , (buildKeypressEvent SDL.KeycodeRight, GameEvent.GE M.Right, set (gameStatePlayerL . playerPositionL) (Point{x = 50 + M.stepSize, y = 50}))
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
