module Game.Level1 (gameState) where

import Foreign.C.Types (CInt)
import GameState (GameState (..))
import Graphics.Point (Point (..))
import Model.Level (Level(..))
import Model.Player (Player (..))
import Model.Room (Room (..))

gameState :: (CInt, CInt) -> GameState
gameState (ww, wh) =
    let p = Player{position = Point{x = ww `div` 2, y = wh `div` 2}}
     in GameState
            { player = p
            , levels =
                [ Level
                    { rooms = [room1]
                    }
                ]
            }

room1 :: Room
room1 = undefined

-- Room {
--                   walls = [
--                       buildWall
--                           ]
--                    }
