module Model.Player (Player (..), playerPositionL) where

import Control.Lens
import Graphics.Color (Color (..))
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import Render.Renderable (Renderable (..))

newtype Player = Player
    { position :: Point
    }
    deriving (Show, Eq)

playerPositionL :: Lens' Player Point
playerPositionL = lens position (\player p -> player{position = p})

instance Renderable Player where
    render (Player{position = p}) renderer =
        drawRectangle renderer (Rectangle{topLeftCorner = p, width = 20, height = 20, fillColor = (Color{red = 0, green = 0, blue = 255, alpha = 255}), borderColor = Nothing})
