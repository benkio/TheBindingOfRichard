module GameState (GameState (..), transformGameState, gameStatePlayerL, gameStateLevelsL) where

import Control.Lens hiding (Level, levels)
import Controls (Controls (..))
import GameEvent (GameEvent (..), toGameEvent)

import CollisionDetection (isWithinRoom)
import Graphics.Window (windowToBlack)
import Model.Level (Level (..), levelRoomsL)
import qualified Model.Level as L
import Model.Move (movePoint)
import Model.Player (Player (..), playerPositionL, playerPositionLevelIdL, playerPositionPositionL, playerPositionRoomIdL)
import Model.Room (Room (..), toInnerRoom)
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

-- TODO: change this. it returns Nothing if nothing changed or GameExit if the game terminates!
transformGameState :: [Event] -> Controls -> GameState -> Maybe GameState
transformGameState evs controls gs =
    foldl (\mst e -> mst >>= \st -> transformGameState' e controls st) (Just gs) evs

instance Renderable GameState where
    render (GameState{player = p, levels = ls}) renderer = do
        windowToBlack renderer
        mapM_ (`render` renderer) ls
        render p renderer
        present renderer

isLegalState :: GameState -> Bool
isLegalState gs = maybe False (isWithinRoom pp . toInnerRoom) mr
  where
    plid = view (gameStatePlayerL . playerPositionL . playerPositionLevelIdL) gs
    prid = view (gameStatePlayerL . playerPositionL . playerPositionRoomIdL) gs
    pp = view (gameStatePlayerL . playerPositionL . playerPositionPositionL) gs
    mr = preview (gameStateLevelsL . filtered ((== plid) . levelId) . levelRoomsL . filtered ((prid ==) . roomId)) gs
