module GameEvent (GameEvent (..), toGameEvent) where

import Controls (Controls (..))
import Move (Move (..))
import SDL (Event, keysymKeycode)
import SDL.Event (EventPayload (..), InputMotion (Pressed), eventPayload, keyboardEventKeyMotion, keyboardEventKeysym)
import SDL.Input.Keyboard.Codes

data GameEvent = GE Move | Quit

toGameEvent :: Event -> Controls -> GameEvent
toGameEvent event controls
  | eventIsKeyPressed (up controls) event = GE Move.Up
  | eventIsKeyPressed (down controls) event = GE Move.Down
  | eventIsKeyPressed (left controls) event = GE Move.Left
  | eventIsKeyPressed (right controls) event = GE Move.Right
  | eventIsKeyPressed (quit controls) event || eventIsCloseWindow event = Quit
  | otherwise = GE Rest

eventIsKeyPressed :: Keycode -> Event -> Bool
eventIsKeyPressed keyCode event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed
        && keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
    _ -> False

eventIsCloseWindow :: Event -> Bool
eventIsCloseWindow = go . eventPayload
  where go (WindowClosedEvent _) = True
        go _ = False
