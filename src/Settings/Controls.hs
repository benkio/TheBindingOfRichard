module Settings.Controls (Controls (..), defaultControls) where

import SDL.Input.Keyboard.Codes

data Controls = Controls
    { up :: Keycode
    , down :: Keycode
    , left :: Keycode
    , right :: Keycode
    , quit :: Keycode
    }

defaultControls :: Controls
defaultControls =
    Controls
        { up = KeycodeUp
        , down = KeycodeDown
        , left = KeycodeLeft
        , right = KeycodeRight
        , quit = KeycodeQ
        }
