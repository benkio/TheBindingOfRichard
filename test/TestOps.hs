module TestOps (eventMap, arrowEventMap, quitEventMap, testGameState, buildKeypressEvent) where

import Control.Lens
import GameEvent (GameEvent (..))
import qualified GameState as G (GameState (..), gameStatePlayerL)
import Graphics.Point (Point (..))
import qualified Model.Move as M
import Model.Player (Player (..), playerPositionL)
import qualified SDL

testGameState :: G.GameState
testGameState = G.GameState{G.player = Player{position = Point{x = 50, y = 50}}, G.levels = []}

eventMap :: [(SDL.Event, GameEvent, G.GameState -> G.GameState)]
eventMap =
    arrowEventMap ++ quitEventMap

quitEventMap :: [(SDL.Event, GameEvent, G.GameState -> G.GameState)]
quitEventMap = [(buildKeypressEvent SDL.KeycodeQ, GameEvent.Quit, id)]

arrowEventMap :: [(SDL.Event, GameEvent, G.GameState -> G.GameState)]
arrowEventMap =
    [ (buildKeypressEvent SDL.KeycodeUp, GameEvent.GE M.Up, set (G.gameStatePlayerL . playerPositionL) (Point{x = 50, y = 50 - M.stepSize}))
    , (buildKeypressEvent SDL.KeycodeDown, GameEvent.GE M.Down, set (G.gameStatePlayerL . playerPositionL) (Point{x = 50, y = 50 + M.stepSize}))
    , (buildKeypressEvent SDL.KeycodeLeft, GameEvent.GE M.Left, set (G.gameStatePlayerL . playerPositionL) (Point{x = 50 - M.stepSize, y = 50}))
    , (buildKeypressEvent SDL.KeycodeRight, GameEvent.GE M.Right, set (G.gameStatePlayerL . playerPositionL) (Point{x = 50 + M.stepSize, y = 50}))
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
