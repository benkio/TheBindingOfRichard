module Game.GameEvent (GameEvent (..), toGameEvent) where

import qualified Game.Model.Move as M (Move (..))
import SDL (Event, keysymKeycode)
import SDL.Event (EventPayload (..), InputMotion (Pressed), eventPayload, keyboardEventKeyMotion, keyboardEventKeysym)
import SDL.Input.Keyboard.Codes
import Settings.Controls (Controls (..))

data GameEvent = GE M.Move | Quit deriving (Eq, Show)

toGameEvent :: Event -> Controls -> GameEvent
toGameEvent event controls
    | eventIsKeyPressed (up controls) event = GE M.Up
    | eventIsKeyPressed (down controls) event = GE M.Down
    | eventIsKeyPressed (left controls) event = GE M.Left
    | eventIsKeyPressed (right controls) event = GE M.Right
    | eventIsKeyPressed (quit controls) event || eventIsCloseWindow event = Quit
    | otherwise = GE M.Rest

eventIsKeyPressed :: Keycode -> Event -> Bool
eventIsKeyPressed keyCode event =
    case eventPayload event of
        KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
                && keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
        _ -> False

eventIsCloseWindow :: Event -> Bool
eventIsCloseWindow = go . eventPayload
  where
    go (WindowClosedEvent _) = True
    go _ = False
