module GameState (GameState (..), initialGameState, transformGameState) where

import Controls (Controls (..))
import Foreign.C.Types (CInt)
import GameEvent (GameEvent (..), toGameEvent)
import Move (moveToValueX, moveToValueY)
import SDL (Event, keysymKeycode)
import SDL.Event (EventPayload (..), InputMotion (Pressed), eventPayload, keyboardEventKeyMotion, keyboardEventKeysym)
import SDL.Input.Keyboard.Codes

data GameState = GameState
  { x :: CInt,
    y :: CInt
  }

initialGameState :: GameState
initialGameState = GameState {x = 0, y = 0}

transformGameState' :: GameState -> GameEvent -> Maybe GameState
transformGameState' gs (GE move) = Just $ gs {x = x gs + moveToValueX move, y = y gs + moveToValueY move}
transformGameState' _ Quit = Nothing

transformGameState :: Event -> Controls -> GameState -> Maybe GameState
transformGameState ev controls gs = transformGameState' gs $ toGameEvent ev controls
