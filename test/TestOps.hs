module TestOps (eventMap, arrowEventMap, quitEventMap, testGameState, buildKeypressEvent, testWindowSize) where

import Control.Lens
import Foreign.C.Types (CInt)
import qualified Game.GameState as G (GameState (..), gameStatePlayerL)
import Game.Model.Player (Player (..), PlayerPosition (..), playerPositionL, playerPositionPositionL)
import Graphics.Point (Point (..))
import Init.GameResources (texturesLocations)
import Model.Event (Event (..))
import qualified Model.Move as M
import qualified SDL

testWindowSize :: (CInt, CInt)
testWindowSize = (100, 100)

testGameState :: G.GameState
testGameState =
    G.GameState
        { G.player =
            Player
                { playerPosition = PlayerPosition{position = Point{x = 50, y = 50}, roomId = 0, levelId = 0}
                , playerTextureLocation = head texturesLocations
                }
        , G.levels = []
        }

eventMap :: [(SDL.Event, Event, G.GameState -> G.GameState)]
eventMap =
    arrowEventMap ++ quitEventMap

quitEventMap :: [(SDL.Event, Event, G.GameState -> G.GameState)]
quitEventMap = [(buildKeypressEvent SDL.KeycodeQ, Model.Event.Quit, id)]

arrowEventMap :: [(SDL.Event, Event, G.GameState -> G.GameState)]
arrowEventMap =
    [ (buildKeypressEvent SDL.KeycodeUp, Model.Event.GE M.Up, set (G.gameStatePlayerL . playerPositionL . playerPositionPositionL) (Point{x = 50, y = 50 - M.stepSize}))
    , (buildKeypressEvent SDL.KeycodeDown, Model.Event.GE M.Down, set (G.gameStatePlayerL . playerPositionL . playerPositionPositionL) (Point{x = 50, y = 50 + M.stepSize}))
    , (buildKeypressEvent SDL.KeycodeLeft, Model.Event.GE M.Left, set (G.gameStatePlayerL . playerPositionL . playerPositionPositionL) (Point{x = 50 - M.stepSize, y = 50}))
    , (buildKeypressEvent SDL.KeycodeRight, Model.Event.GE M.Right, set (G.gameStatePlayerL . playerPositionL . playerPositionPositionL) (Point{x = 50 + M.stepSize, y = 50}))
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
