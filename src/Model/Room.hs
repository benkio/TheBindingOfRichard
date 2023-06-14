module Model.Room (Room (..), roomWallsL) where

import Control.Lens
import Model.Wall (Wall (..))
import Render.Renderable

newtype Room = Room
    { walls :: [Wall]
    }
    deriving (Show, Eq)

roomWallsL :: Lens' Room [Wall]
roomWallsL = lens walls (\room ws -> room{walls = ws})

instance Renderable Room where
    render (Room{walls = ws}) renderer =
        mapM_ (`render` renderer) ws
