module Settings.Controls (Controls, defaultControls) where

import Data.Map (Map, fromList)
import SDL.Input.Keyboard.Codes

type Controls = Map Char Keycode

defaultControls :: Controls
defaultControls =
    fromList
        [ ('↑', KeycodeUp)
        , ('↓', KeycodeDown)
        , ('←', KeycodeLeft)
        , ('→', KeycodeRight)
        , ('q', KeycodeQ)
        , ('⎆', KeycodeReturn)
        ]
