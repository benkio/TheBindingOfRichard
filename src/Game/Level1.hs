module Game.Level1 (gameState) where

import Foreign.C.Types (CInt)
import GameState (GameState (..))
import Graphics.Color (Color (..))
import Graphics.Point (Point (..))
import Model.Level (Level (..))
import qualified Model.Player as P (Player (..), PlayerPosition (..))
import Model.Room (Room (..))
import Model.Wall (Wall (..))

gameState :: (CInt, CInt) -> GameState
gameState (ww, wh) =
    let p = P.Player{P.playerPosition = P.PlayerPosition{P.position = Point{x = ww `div` 2, y = wh `div` 2}, P.roomId = 0}}
     in GameState
            { player = p
            , levels =
                [ Level
                    { rooms = [room1 ww wh]
                    }
                ]
            }

room1 :: CInt -> CInt -> Room
room1 ww wh =
    Room
        { roomId = 0
        , walls =
            [ Wall{start = Point{x = wwStep, y = whStep}, end = Point{x = wwStep, y = whStep * 6}, thickness = 10} -- left
            , Wall{start = Point{x = wwStep, y = whStep * 6}, end = Point{x = wwStep * 6, y = whStep * 6}, thickness = 10} -- bottom
            , Wall{start = Point{x = wwStep * 6, y = whStep}, end = Point{x = wwStep * 6, y = whStep * 6}, thickness = 10} -- right
            , Wall{start = Point{x = wwStep, y = whStep}, end = Point{x = wwStep * 6, y = whStep}, thickness = 10} -- top
            ]
        , backgroundColor = Color{red = 200, green = 160, blue = 130, alpha = 255}
        }
  where
    wwStep = ww `div` 7
    whStep = wh `div` 7
