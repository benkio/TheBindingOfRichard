module Model.Player (Player (..), PlayerPosition (..), playerPositionL, playerPositionPositionL, playerPositionRoomIdL) where

import Control.Lens
import Graphics.Color (Color (..))
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import Render.Renderable (Renderable (..))

data PlayerPosition = PlayerPosition
    { position :: Point
    , roomId :: Int
    }
    deriving (Show, Eq)

newtype Player = Player
    { playerPosition :: PlayerPosition
    }
    deriving (Show, Eq)

playerPositionL :: Lens' Player PlayerPosition
playerPositionL = lens playerPosition (\player p -> player{playerPosition = p})

playerPositionPositionL :: Lens' PlayerPosition Point
playerPositionPositionL = lens position (\pp p -> pp{position = p})

playerPositionRoomIdL :: Lens' PlayerPosition Int
playerPositionRoomIdL = lens roomId (\pp i -> pp{roomId = i})

instance Renderable Player where
    render (Player{playerPosition = PlayerPosition{position = p}}) renderer =
        drawRectangle renderer (Rectangle{topLeftCorner = p, width = 20, height = 20, fillColor = (Color{red = 0, green = 0, blue = 255, alpha = 255}), borderColor = Nothing})
