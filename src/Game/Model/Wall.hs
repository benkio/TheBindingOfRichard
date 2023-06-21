module Game.Model.Wall (Wall (..), wallStartL, wallEndL, defaultThickness) where

import Control.Lens
import Foreign.C.Types (CInt)
import Graphics.Color (greyColor)
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import Render.Renderable (Renderable (..))
import Text.Printf

data Wall = Wall
    { start :: !Point
    , end :: !Point
    }
    deriving (Show, Eq)

defaultThickness :: CInt
defaultThickness = 10

wallStartL :: Lens' Wall Point
wallStartL = lens start (\wall s -> wall{start = s})

wallEndL :: Lens' Wall Point
wallEndL = lens end (\wall s -> wall{end = s})

wallToRectangle :: Wall -> Rectangle
wallToRectangle (Wall{start = Point{x = sx, y = sy}, end = Point{x = ex, y = ey}})
    | sy < ey = Rectangle{topLeftCorner = Point{x = sx - (defaultThickness `div` 2), y = sy}, width = defaultThickness, height = ey - sy, fillColor = greyColor, borderColor = Nothing}
    | sx < ex = Rectangle{topLeftCorner = Point{x = sx, y = sy - (defaultThickness `div` 2)}, width = ex - sx, height = defaultThickness, fillColor = greyColor, borderColor = Nothing}
    | ey < sy = Rectangle{topLeftCorner = Point{x = ex - (defaultThickness `div` 2), y = ey}, width = defaultThickness, height = sy - ey, fillColor = greyColor, borderColor = Nothing}
    | ex < sx = Rectangle{topLeftCorner = Point{x = ex, y = ey - (defaultThickness `div` 2)}, width = sx - ex, height = defaultThickness, fillColor = greyColor, borderColor = Nothing}
    | otherwise = error $ printf "Can't build a wall from these 2 points: (%s, %s) - (%s, %s)" (show sx) (show sy) (show ex) (show ey)

instance Renderable Wall where
    render w renderer _ =
        drawRectangle renderer (wallToRectangle w)
