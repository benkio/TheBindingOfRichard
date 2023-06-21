module Game.Physics.CollisionDetection (isWithinRoom) where

import Game.Model.Player (playerSize)
import Game.Model.Room (Room, toRectangle)
import Graphics.Point (Point (..))
import Graphics.Rectangle (height, topLeftCorner, width)

isWithinRoom :: Point -> Room -> Bool
isWithinRoom (Point{x = px, y = py}) r =
    px >= (x . topLeftCorner) rect
        && py >= (y . topLeftCorner) rect
        && (px + playerHalfWidth) < ((+ width rect) . x . topLeftCorner) rect
        && (py + playerHalfHeight) < ((+ height rect) . y . topLeftCorner) rect
  where
    playerHalfWidth = ((`div` 2) . fst) playerSize
    playerHalfHeight = ((`div` 2) . snd) playerSize
    rect = toRectangle r
