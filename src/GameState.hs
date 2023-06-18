module GameState (GameState (..), transformGameState, gameStatePlayerL, gameStateLevelsL) where

import Control.Lens hiding (Level, levels)
import Controls (Controls (..))
import GameEvent (GameEvent (..), toGameEvent)
import Graphics.Window (windowToBlack)
import Model.Level (Level (..))
import qualified Model.Level as L
import Model.Move (movePoint)
import Model.Player (Player (..), playerPositionL)
import Render.Renderable
import SDL (Event, present)

data GameState = GameState
    { player :: Player
    , levels :: [L.Level]
    }
    deriving (Show, Eq)

gameStatePlayerL :: Lens' GameState Player
gameStatePlayerL = lens player (\state p -> state{player = p})

gameStateLevelsL :: Lens' GameState [Level]
gameStateLevelsL = lens levels (\state ls -> state{levels = ls})

transformGameState'' :: GameState -> GameEvent -> Maybe GameState
transformGameState'' gs (GE move) = Just $ over (gameStatePlayerL . playerPositionL) (movePoint move) gs
transformGameState'' _ Quit = Nothing

transformGameState' :: Event -> Controls -> GameState -> Maybe GameState
transformGameState' ev controls gs = transformGameState'' gs $ toGameEvent ev controls

transformGameState :: [Event] -> Controls -> GameState -> Maybe GameState
transformGameState evs controls gs =
    foldl (\mst e -> mst >>= \st -> transformGameState' e controls st) (Just gs) evs

instance Renderable GameState where
    render (GameState{player = p, levels = ls}) renderer = do
        windowToBlack renderer
        mapM_ (`render` renderer) ls
        render p renderer
        present renderer
