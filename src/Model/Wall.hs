module Model.Wall (Wall (..), wallStartL, wallEndL) where

import Control.Lens
import Foreign.C.Types (CInt)
import Graphics.Color (Color (..))
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import Render.Renderable (Renderable (..))
import Text.Printf

data Wall = Wall
    { start :: !Point
    , end :: !Point
    , thickness :: Int
    }
    deriving (Show, Eq)

wallStartL :: Lens' Wall Point
wallStartL = lens start (\wall s -> wall{start = s})

wallEndL :: Lens' Wall Point
wallEndL = lens end (\wall s -> wall{end = s})

wallToRectangle :: Wall -> Rectangle
wallToRectangle (Wall{start = Point{x = sx, y = sy}, end = Point{x = ex, y = ey}, thickness = t})
    | sy < ey = Rectangle{topLeftCorner = Point{x = sx - ((fromIntegral t :: CInt) `div` 2), y = sy}, width = (fromIntegral t :: CInt), height = ey - sy, fillColor = Color{red = 200, green = 200, blue = 200, alpha = 255}, borderColor = Nothing}
    | sx < ex = Rectangle{topLeftCorner = Point{x = sx, y = sy - ((fromIntegral t :: CInt) `div` 2)}, width = ex - sx, height = (fromIntegral t :: CInt), fillColor = Color{red = 200, green = 200, blue = 200, alpha = 255}, borderColor = Nothing}
    | ey < sy = Rectangle{topLeftCorner = Point{x = ex - ((fromIntegral t :: CInt) `div` 2), y = ey}, width = (fromIntegral t :: CInt), height = sy - ey, fillColor = Color{red = 200, green = 200, blue = 200, alpha = 255}, borderColor = Nothing}
    | ex < sx = Rectangle{topLeftCorner = Point{x = ex, y = ey - ((fromIntegral t :: CInt) `div` 2)}, width = sx - ex, height = (fromIntegral t :: CInt), fillColor = Color{red = 200, green = 200, blue = 200, alpha = 255}, borderColor = Nothing}
    | otherwise = error $ printf "Can't build a wall from these 2 points: (%s, %s) - (%s, %s)" (show sx) (show sy) (show ex) (show ey)

instance Renderable Wall where
    render w renderer =
        drawRectangle renderer (wallToRectangle w)
