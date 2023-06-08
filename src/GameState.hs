module GameState (GameState (..), initialGameState, transformGameState) where

import Controls (Controls (..))
import Data.Functor ((<&>))
import GameEvent (GameEvent (..), toGameEvent)
import Graphics.Point (Point (..))
import Graphics.Window (windowSize)
import Move (moveToValueX, moveToValueY)
import SDL (Event)

newtype GameState = GameState
    { position :: Point
    }

initialGameState :: IO GameState
initialGameState = windowSize <&> \(ww, wh) -> GameState{position = Point{x = ww `div` 2, y = wh `div` 2}}

-- TODO: Use Lenses!
transformGameState'' :: GameState -> GameEvent -> Maybe GameState
transformGameState'' gs (GE move) = Just $ gs{position = ((\p -> p{x = x p + moveToValueX move, y = y p + moveToValueY move}) . position) gs}
transformGameState'' _ Quit = Nothing

transformGameState' :: Event -> Controls -> GameState -> Maybe GameState
transformGameState' ev controls gs = transformGameState'' gs $ toGameEvent ev controls

transformGameState :: [Event] -> Controls -> GameState -> Maybe GameState
transformGameState evs controls gs =
    foldl (\mst e -> mst >>= \st -> transformGameState' e controls st) (Just gs) evs
