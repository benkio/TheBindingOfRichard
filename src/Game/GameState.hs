module Game.GameState (GameState (..), transformGameState, gameStatePlayerL, gameStateLevelsL) where

import Control.Lens hiding (Level, levels)
import Game.GameEvent (GameEvent (..), toGameEvent)
import Settings.Controls (Controls (..))

import Game.Model.Level (Level (..), levelRoomsL)
import qualified Game.Model.Level as L
import Game.Model.Move (movePoint)
import Game.Model.Player (Player (..), playerPositionL, playerPositionLevelIdL, playerPositionPositionL, playerPositionRoomIdL)
import Game.Model.Room (Room (..), toInnerRoom)
import Game.Physics.CollisionDetection (isWithinRoom)
import Graphics.Window (windowToBlack)
import Render.Renderable
import SDL (Event, present)

data GameState = GameState
    { player :: Player
    , levels :: [L.Level]
    }
    deriving (Show, Eq)

gameStatePlayerL :: Lens' GameState Player
gameStatePlayerL = lens player (\state p -> state{player = p})

gameStateLevelsL :: Fold GameState Level
gameStateLevelsL = folding levels

transformGameState'' :: GameState -> GameEvent -> Maybe GameState
transformGameState'' gs (GE move)
    | isLegalState gs' = Just gs'
    | otherwise = Just gs
  where
    gs' = over (gameStatePlayerL . playerPositionL . playerPositionPositionL) (movePoint move) gs
transformGameState'' _ Quit = Nothing

transformGameState' :: Event -> Controls -> GameState -> Maybe GameState
transformGameState' ev controls gs = transformGameState'' gs $ toGameEvent ev controls

transformGameState :: [Event] -> Controls -> GameState -> Maybe GameState
transformGameState evs controls gs =
    foldl (\mst e -> mst >>= \st -> transformGameState' e controls st) (Just gs) evs

instance Renderable GameState where
    render (GameState{player = p, levels = ls}) renderer gr = do
        windowToBlack renderer
        mapM_ (\l -> render l renderer gr) ls
        render p renderer gr
        present renderer

isLegalState :: GameState -> Bool
isLegalState gs = maybe False (isWithinRoom pp . toInnerRoom) mr
  where
    plid = view (gameStatePlayerL . playerPositionL . playerPositionLevelIdL) gs
    prid = view (gameStatePlayerL . playerPositionL . playerPositionRoomIdL) gs
    pp = view (gameStatePlayerL . playerPositionL . playerPositionPositionL) gs
    mr = preview (gameStateLevelsL . filtered ((== plid) . levelId) . levelRoomsL . filtered ((prid ==) . roomId)) gs
