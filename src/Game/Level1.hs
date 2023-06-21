module Game.Level1 (gameState) where

import GameSetup (GameSetup (..))
import GameState (GameState (..))
import Graphics.Point (Point (..))
import Model.Level (Level (..))
import qualified Model.Player as P (Player (..), PlayerPosition (..))
import Model.Room (standardRoom)

gameState :: GameSetup -> GameState
gameState (GameSetup{windowSize = (ww, wh)}) =
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
