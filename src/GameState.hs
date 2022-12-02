module GameState (GameState (..), initialGameState, transformGameState) where

import Controls (Controls (..))
import Foreign.C.Types (CInt)
import GameEvent (GameEvent (..), toGameEvent)
import Graphics.Point (Point (..))
import Move (moveToValueX, moveToValueY)
import SDL (Event, keysymKeycode)
import SDL.Event (EventPayload (..), InputMotion (Pressed), eventPayload, keyboardEventKeyMotion, keyboardEventKeysym)
import SDL.Input.Keyboard.Codes

data GameState = GameState
  { position :: Point
  }

initialGameState :: GameState
initialGameState = GameState {position = Point {x = 0, y = 0}}

-- TODO: Use Lenses!
transformGameState'' :: GameState -> GameEvent -> Maybe GameState
transformGameState'' gs (GE move) = Just $ gs {position = ((\p -> p {x = x p + moveToValueX move, y = y p + moveToValueY move}) . position) gs}
transformGameState'' _ Quit = Nothing

transformGameState' :: Event -> Controls -> GameState -> Maybe GameState
transformGameState' ev controls gs = transformGameState'' gs $ toGameEvent ev controls

transformGameState :: [Event] -> Controls -> GameState -> Maybe GameState
transformGameState evs controls gs =
  foldl (\mst e -> mst >>= \st -> transformGameState' e controls st) (Just gs) evs
