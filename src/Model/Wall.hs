module Model.Wall (Wall (..)-- , wallRectangleL
                  ) where

import Graphics.Point (Point (..))
--import Render.Renderable (Renderable (..))
--import Control.Lens
--import Graphics.Rectangle (Rectangle (..), drawRectangle)
--import Text.Printf

data Wall = Wall {
  start :: !Point,
  end :: !Point
  } deriving (Show, Eq)

-- wallRectangleL :: Lens' Wall Rectangle
-- wallRectangleL = lens rectangle (\wall r -> wall{rectangle = r})

-- instance Renderable Wall where
--     render (Wall{rectangle = r}) renderer =
--       undefined-- drawRectangle renderer r
