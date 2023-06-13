module GameState (GameState (..), initialGameState, transformGameState, gameStatePlayerL) where

import Foreign.C.Types (CInt)

import Control.Lens
import Controls (Controls (..))
import GameEvent (GameEvent (..), toGameEvent)
import Graphics.Point (Point (..))
import Model.Move (movePoint)
import Model.Player (Player (..), playerPositionL)
import SDL (Event)

newtype GameState = GameState
    { player :: Player
    }
    deriving (Eq, Show)

gameStatePlayerL :: Lens' GameState Player
gameStatePlayerL = lens player (\state p -> state{player = p})

initialGameState :: (CInt, CInt) -> GameState
initialGameState (ww, wh) = GameState{player = Player{position = Point{x = ww `div` 2, y = wh `div` 2}}}

transformGameState'' :: GameState -> GameEvent -> Maybe GameState
transformGameState'' gs (GE move) = Just $ over (gameStatePlayerL . playerPositionL) (movePoint move) gs
transformGameState'' _ Quit = Nothing

transformGameState' :: Event -> Controls -> GameState -> Maybe GameState
transformGameState' ev controls gs = transformGameState'' gs $ toGameEvent ev controls

transformGameState :: [Event] -> Controls -> GameState -> Maybe GameState
transformGameState evs controls gs =
    foldl (\mst e -> mst >>= \st -> transformGameState' e controls st) (Just gs) evs
