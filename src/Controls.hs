module Controls (Controls (..), defaultControls) where

import SDL.Input.Keyboard.Codes

data Controls = Controls
  { up :: Keycode,
    down :: Keycode,
    left :: Keycode,
    right :: Keycode,
    quit :: Keycode -- TODO: close on window X
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
