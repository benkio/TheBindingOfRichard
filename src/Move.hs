module Move (Move (..), movePoint, stepSize) where

import Foreign.C.Types (CInt)
import Graphics.Point (Point (..))

data Move
    = Up
    | Down
    | Left
    | Right
    | Rest
    deriving Enum

stepSize :: CInt
stepSize = 10

moveToValueX :: Move -> CInt
moveToValueX Move.Left = -stepSize
moveToValueX Move.Right = stepSize
moveToValueX _ = 0

moveToValueY :: Move -> CInt
moveToValueY Move.Up = -stepSize
moveToValueY Move.Down = stepSize
moveToValueY _ = 0

movePoint :: Move -> Point -> Point
movePoint move p = p{x = x p + moveToValueX move, y = y p + moveToValueY move}
