module Model.Level (Level (..), levelRoomsL) where

import qualified Control.Lens as L
import Model.Room (Room (..))

newtype Level = Level
    { rooms :: [Room]
    }
    deriving (Show, Eq)

levelRoomsL :: L.Lens' Level [Room]
levelRoomsL = L.lens rooms (\level rs -> level{rooms = rs})
