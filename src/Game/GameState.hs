module Game.GameState (GameState (..), transformGameState, gameStatePlayerL, gameStateLevelsL) where

import Control.Lens hiding (Level, levels)
import Model.Event (Event (..), toEventDefaultControl)
import Settings.Controls (Controls)

import Game.Model.Level (Level (..), levelRoomsL)
import qualified Game.Model.Level as L
import Game.Model.Player (Player (..), playerPositionL, playerPositionLevelIdL, playerPositionPositionL, playerPositionRoomIdL)
import Game.Model.Room (Room (..), toInnerRoom)
import Game.Physics.CollisionDetection (isWithinRoom)
import Graphics.Window (windowToBlack)
import Model.Move (movePoint)
import Render.Renderable
import SDL (present)
import qualified SDL as S (Event)

data GameState = GameState
    { player :: Player
    , levels :: [L.Level]
    }
    deriving (Show, Eq)

gameStatePlayerL :: Lens' GameState Player
gameStatePlayerL = lens player (\state p -> state{player = p})

gameStateLevelsL :: Fold GameState Level
gameStateLevelsL = folding levels

transformGameState'' :: GameState -> Event -> Maybe GameState
transformGameState'' gs (GE move)
    | isLegalState gs' = Just gs'
    | otherwise = Just gs
  where
    gs' = over (gameStatePlayerL . playerPositionL . playerPositionPositionL) (movePoint move) gs
transformGameState'' _ Quit = Nothing
transformGameState'' _ Interact = Nothing -- TODO: Implement

transformGameState' :: S.Event -> Controls -> GameState -> Maybe GameState
transformGameState' ev controls gs =
    case toEventDefaultControl ev controls of
        Just e -> transformGameState'' gs e
        Nothing -> Just gs

transformGameState :: [S.Event] -> Controls -> GameState -> Maybe GameState
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
