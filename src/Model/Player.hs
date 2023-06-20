module Model.Player (Player (..), PlayerPosition (..), playerPositionL, playerPositionPositionL, playerPositionRoomIdL, playerPositionLevelIdL, playerSize) where

import Control.Lens
import Foreign.C.Types (CInt)
import Graphics.Color (blueColor)
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import Render.Renderable (Renderable (..))

data PlayerPosition = PlayerPosition
    { position :: Point
    , roomId :: Int
    , levelId :: Int
    }
    deriving (Show, Eq)

newtype Player = Player
    { playerPosition :: PlayerPosition
    }
    deriving (Show, Eq)

playerSize :: (CInt, CInt)
playerSize = (20, 20)

playerPositionL :: Lens' Player PlayerPosition
playerPositionL = lens playerPosition (\player p -> player{playerPosition = p})

playerPositionPositionL :: Lens' PlayerPosition Point
playerPositionPositionL = lens position (\pp p -> pp{position = p})

playerPositionRoomIdL :: Lens' PlayerPosition Int
playerPositionRoomIdL = lens roomId (\pp i -> pp{roomId = i})

playerPositionLevelIdL :: Lens' PlayerPosition Int
playerPositionLevelIdL = lens levelId (\pp i -> pp{levelId = i})

instance Renderable Player where
    render (Player{playerPosition = PlayerPosition{position = p}}) renderer =
        drawRectangle renderer (Rectangle{topLeftCorner = p, width = fst playerSize, height = snd playerSize, fillColor = blueColor, borderColor = Nothing})
