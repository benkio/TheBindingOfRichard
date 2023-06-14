module Game.Level1 (gameState) where

import Foreign.C.Types (CInt)
import GameState (GameState (..))
import Graphics.Point (Point (..))
import Model.Level (Level (..))
import Model.Player (Player (..))
import Model.Room (Room (..))
import Model.Wall (Wall (..))

gameState :: (CInt, CInt) -> GameState
gameState (ww, wh) =
    let p = Player{position = Point{x = ww `div` 2, y = wh `div` 2}}
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
        { walls =
            [ Wall{start = Point{x = wwStep, y = whStep}, end = Point{x = wwStep, y = whStep * 3}, thickness = 10} -- left
            , Wall{start = Point{x = wwStep * 3, y = whStep}, end = Point{x = wwStep * 3, y = whStep * 3}, thickness = 10} -- right
            , Wall{start = Point{x = wwStep, y = whStep}, end = Point{x = wwStep * 3, y = whStep}, thickness = 10} -- top
            , Wall{start = Point{x = wwStep, y = whStep * 3}, end = Point{x = wwStep * 3, y = whStep * 3}, thickness = 10} -- bottom
            ]
        }
  where
    wwStep = ww `div` 4
    whStep = wh `div` 4
