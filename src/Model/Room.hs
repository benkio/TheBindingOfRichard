module Model.Room (Room (..), roomWallsL) where

import Control.Lens
import Model.Wall (Wall (..))

newtype Room = Room
    { walls :: [Wall]
    }
    deriving (Show, Eq)

roomWallsL :: Lens' Room [Wall]
roomWallsL = lens walls (\room ws -> room{walls = ws})
