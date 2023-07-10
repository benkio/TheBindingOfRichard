module Model.Move (Move (..), movePoint, stepSize) where

import Foreign.C.Types (CInt)
import Graphics.Point (Point (..))
import Prelude hiding (Left, Right)

data Move
    = Up
    | Down
    | Left
    | Right
    deriving (Show, Enum, Eq)

stepSize :: CInt
stepSize = 10

moveToValueX :: Move -> CInt
moveToValueX Left = -stepSize
moveToValueX Right = stepSize
moveToValueX _ = 0

moveToValueY :: Move -> CInt
moveToValueY Up = -stepSize
moveToValueY Down = stepSize
moveToValueY _ = 0

movePoint :: Move -> Point -> Point
movePoint move p = p{x = x p + moveToValueX move, y = y p + moveToValueY move}
