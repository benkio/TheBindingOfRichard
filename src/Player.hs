module Player (Player (..), playerPositionL) where

import Control.Lens
import Graphics.Point (Point (..))

newtype Player = Player
    { position :: Point
    }
    deriving (Show, Eq)

playerPositionL :: Lens' Player Point
playerPositionL = lens position (\player p -> player{position = p})
