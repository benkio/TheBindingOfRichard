module Move (Move (..), moveToValueX, moveToValueY) where

import Foreign.C.Types (CInt)

data Move
  = Up
  | Down
  | Left
  | Right
  | Rest

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
