module GameState (GameState (..), transformGameState, gameStatePlayerL) where

import Control.Lens
import Controls (Controls (..))
import GameEvent (GameEvent (..), toGameEvent)
import qualified Model.Level as L
import Model.Move (movePoint)
import Model.Player (Player (..), playerPositionL)
import SDL (Event)

data GameState = GameState
    { player :: Player
    , levels :: [L.Level]
    }
    deriving (Show, Eq)

gameStatePlayerL :: Lens' GameState Player
gameStatePlayerL = lens player (\state p -> state{player = p})

transformGameState'' :: GameState -> GameEvent -> Maybe GameState
transformGameState'' gs (GE move) = Just $ over (gameStatePlayerL . playerPositionL) (movePoint move) gs
transformGameState'' _ Quit = Nothing

transformGameState' :: Event -> Controls -> GameState -> Maybe GameState
transformGameState' ev controls gs = transformGameState'' gs $ toGameEvent ev controls

transformGameState :: [Event] -> Controls -> GameState -> Maybe GameState
transformGameState evs controls gs =
    foldl (\mst e -> mst >>= \st -> transformGameState' e controls st) (Just gs) evs
