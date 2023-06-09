module GameState (GameState (..), initialGameState, transformGameState) where

import Control.Lens
import Controls (Controls (..))
import GameEvent (GameEvent (..), toGameEvent)
import Graphics.Point (Point (..))
import Graphics.Window (windowSize)
import Move (movePoint)
import SDL (Event)

newtype GameState = GameState
    { position :: Point
    }

positionL :: Lens' GameState Point
positionL = lens position (\state p -> state{position = p})

initialGameState :: IO GameState
initialGameState = windowSize <&> \(ww, wh) -> GameState{position = Point{x = ww `div` 2, y = wh `div` 2}}

transformGameState'' :: GameState -> GameEvent -> Maybe GameState
transformGameState'' gs (GE move) = Just $ over positionL (movePoint move) gs
transformGameState'' _ Quit = Nothing

transformGameState' :: Event -> Controls -> GameState -> Maybe GameState
transformGameState' ev controls gs = transformGameState'' gs $ toGameEvent ev controls

transformGameState :: [Event] -> Controls -> GameState -> Maybe GameState
transformGameState evs controls gs =
    foldl (\mst e -> mst >>= \st -> transformGameState' e controls st) (Just gs) evs
