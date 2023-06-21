module Game.Level.Level1 (gameState) where

import Foreign.C.Types (CInt)
import Game.GameState (GameState (..))
import Game.Model.Level (Level (..))
import qualified Game.Model.Player as P (Player (..), PlayerPosition (..))
import Game.Model.Room (standardRoom)
import Graphics.Point (Point (..))

gameState :: (CInt, CInt) -> GameState
gameState (ww, wh) =
    let p =
            P.Player
                { P.playerPosition = P.PlayerPosition{P.position = Point{x = ww `div` 2, y = wh `div` 2}, P.roomId = 0, P.levelId = 0}
                , P.playerTextureLocation = "./image/richard.png"
                }
     in GameState
            { player = p
            , levels =
                [ Level
                    { levelId = 0
                    , rooms = [standardRoom ww wh]
                    }
                ]
            }
