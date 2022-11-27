module Move (Move(..), moveToValueX, moveToValueY)where

import Foreign.C.Types (CInt)
import SDL (Event, keysymKeycode)
import SDL.Event (EventPayload (..), InputMotion (Pressed), eventPayload, keyboardEventKeyMotion, keyboardEventKeysym)
import SDL.Input.Keyboard.Codes


data Move
  = Up
  | Down
  | Left
  | Right
  | Rest

stepSize :: CInt
stepSize = 1

moveToValueX :: Move -> CInt
moveToValueX Move.Left = -stepSize
moveToValueX Move.Right = stepSize
moveToValueX _ = 0

moveToValueY :: Move -> CInt
moveToValueY Move.Up = -stepSize
moveToValueY Move.Down = stepSize
moveToValueY _ = 0
