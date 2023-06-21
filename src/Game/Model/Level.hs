module Game.Model.Level (Level (..), levelRoomsL, levelLevelIdL) where

import qualified Control.Lens as L
import Game.Model.Room (Room (..))
import Render.Renderable

data Level = Level
    { levelId :: Int
    , rooms :: [Room]
    }
    deriving (Show, Eq)

levelRoomsL :: L.Fold Level Room
levelRoomsL = L.folding rooms

levelLevelIdL :: L.Lens' Level Int
levelLevelIdL = L.lens levelId (\level i -> level{levelId = i})

instance Renderable Level where
    render (Level{rooms = rs}) renderer gr =
        mapM_ (\r -> render r renderer gr) rs
