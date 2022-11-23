module Controls where

import Foreign.C.Types (CInt)
import SDL (Event, keysymKeycode)
import SDL.Event (EventPayload (..), InputMotion (Pressed), eventPayload, keyboardEventKeyMotion, keyboardEventKeysym)
import SDL.Input.Keyboard.Codes

data Controls = Controls
  { up :: Keycode,
    down :: Keycode,
    left :: Keycode,
    right :: Keycode,
    quit :: Keycode
  }

data GameEvent = GE Move | Quit

data GameState = GameState
  { x :: CInt,
    y :: CInt
  }

data Move
  = Up
  | Down
  | Left
  | Right
  | Rest

defaultControls :: Controls
defaultControls =
  Controls
    { up = KeycodeUp,
      down = KeycodeDown,
      left = KeycodeLeft,
      right = KeycodeRight,
      quit = KeycodeQ
    }

stepSize :: CInt
stepSize = 1

moveToValueX :: Move -> CInt
moveToValueX Controls.Left = 1
moveToValueX Controls.Right = -1
moveToValueX _ = 0

moveToValueY :: Move -> CInt
moveToValueY Controls.Up = 1
moveToValueY Controls.Down = -1
moveToValueY _ = 0

toGameEvent :: Event -> GameEvent
toGameEvent event
  | eventIsKeyPressed (up defaultControls) event = GE Controls.Up
  | eventIsKeyPressed (down defaultControls) event = GE Controls.Down
  | eventIsKeyPressed (left defaultControls) event = GE Controls.Left
  | eventIsKeyPressed (right defaultControls) event = GE Controls.Right
  | eventIsKeyPressed (quit defaultControls) event = Controls.Quit
  | otherwise = GE Rest

eventIsKeyPressed :: Keycode -> Event -> Bool
eventIsKeyPressed keyCode event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed
        && keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
    _ -> False

transformGameState :: Event -> GameState -> GameState
transformGameState = undefined
