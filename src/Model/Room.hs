module Model.Room (Room (..), roomWallsL) where

import Control.Lens
import Data.Int (Int16)
import Data.List (nub)
import Data.Vector.Storable (fromList)
import Debug.Trace
import Graphics.Color (Color (..), colorToV4)
import Graphics.Point (Point (..))
import Model.Wall (Wall (..))
import Render.Renderable
import SDL.Primitive (fillPolygon)

data Room = Room
    { walls :: [Wall]
    , backgroundColor :: Color
    }
    deriving (Show, Eq)

roomWallsL :: Lens' Room [Wall]
roomWallsL = lens walls (\room ws -> room{walls = ws})

instance Renderable Room where
    render (Room{walls = ws, backgroundColor = bgc}) renderer = do
        fillPolygon renderer (fromList xs') (fromList ys') (colorToV4 bgc)
        mapM_ (`render` renderer) ws
      where
        xs = concatMap (\w -> [(fromIntegral ((x . start) w) :: Int16), (fromIntegral ((x . end) w) :: Int16)]) ws
        ys = concatMap (\w -> [(fromIntegral ((y . start) w) :: Int16), (fromIntegral ((y . end) w) :: Int16)]) ws
        (xs', ys') = unzip $ nub $ zip xs ys
