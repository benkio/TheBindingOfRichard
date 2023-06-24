module Model.Event (Event (..), toEventDefaultControl) where

import qualified Data.Map as M (lookup)
import qualified Model.Move as M (Move (..))
import SDL (keysymKeycode)
import qualified SDL as S (Event)
import SDL.Event (EventPayload (..), InputMotion (Pressed), eventPayload, keyboardEventKeyMotion, keyboardEventKeysym)
import SDL.Input.Keyboard.Codes
import Settings.Controls (Controls)

data Event = GE M.Move | Interact | Quit deriving (Eq, Show)

toEventDefaultControl :: S.Event -> Controls -> Maybe Event
toEventDefaultControl event controls
    | eventIsKeyPressed (M.lookup '↑' controls) event = Just $ GE M.Up
    | eventIsKeyPressed (M.lookup '↓' controls) event = Just $ GE M.Down
    | eventIsKeyPressed (M.lookup '←' controls) event = Just $ GE M.Left
    | eventIsKeyPressed (M.lookup '→' controls) event = Just $ GE M.Right
    | eventIsKeyPressed (M.lookup 'q' controls) event || eventIsCloseWindow event = Just Quit
    | otherwise = Nothing

eventIsKeyPressed :: Maybe Keycode -> S.Event -> Bool
eventIsKeyPressed (Just keyCode) event =
    case eventPayload event of
        KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
                && keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
        _ -> False
eventIsKeyPressed Nothing _ = False

eventIsCloseWindow :: S.Event -> Bool
eventIsCloseWindow = go . eventPayload
  where
    go (WindowClosedEvent _) = True
    go _ = False
