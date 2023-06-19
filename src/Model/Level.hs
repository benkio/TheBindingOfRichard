module Model.Level (Level (..), levelRoomsL, levelLevelIdL) where

import qualified Control.Lens as L
import Model.Room (Room (..))
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
    render (Level{rooms = rs}) renderer =
        mapM_ (`render` renderer) rs
