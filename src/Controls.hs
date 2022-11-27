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

defaultControls :: Controls
defaultControls =
  Controls
    { up = KeycodeUp,
      down = KeycodeDown,
      left = KeycodeLeft,
      right = KeycodeRight,
      quit = KeycodeQ
    }
