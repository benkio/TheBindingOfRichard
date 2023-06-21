module Model.Room (Room (..), roomWallsL, standardRoom, roomTopLeftCornerL, roomBottomLeftCornerL, roomTopRightCornerL, roomBottomRightCornerL, toRectangle, toInnerRoom) where

import Control.Lens

import Foreign.C.Types (CInt)
import Graphics.Color (Color (..), lightBrownColor)
import Graphics.Point (Point (..))
import qualified Graphics.Rectangle as R (Rectangle (..), drawRectangle)
import Model.Wall (Wall (..), defaultThickness)
import Render.Renderable

data Room = Room
    { roomId :: Int
    , topLeftCorner :: Point
    , topRightCorner :: Point
    , bottomLeftCorner :: Point
    , bottomRightCorner :: Point
    , backgroundColor :: Color
    }
    deriving (Show, Eq)

roomWalls :: Room -> [Wall]
roomWalls r =
    [ Wall{start = topLeftCorner r, end = bottomLeftCorner r}
    , Wall{start = bottomLeftCorner r, end = bottomRightCorner r}
    , Wall{start = topRightCorner r, end = bottomRightCorner r}
    , Wall{start = topLeftCorner r, end = topRightCorner r}
    ]

roomWallsL :: Optic' (->) (Const [Wall]) Room [Wall]
roomWallsL = to roomWalls

roomTopLeftCornerL :: Lens' Room Point
roomTopLeftCornerL = lens topLeftCorner (\room tlc -> room{topLeftCorner = tlc})
roomTopRightCornerL :: Lens' Room Point
roomTopRightCornerL = lens topRightCorner (\room trc -> room{topRightCorner = trc})
roomBottomLeftCornerL :: Lens' Room Point
roomBottomLeftCornerL = lens bottomLeftCorner (\room blc -> room{bottomLeftCorner = blc})
roomBottomRightCornerL :: Lens' Room Point
roomBottomRightCornerL = lens bottomRightCorner (\room brc -> room{bottomRightCorner = brc})

-- Room without the thickness of the walls
toInnerRoom :: Room -> Room
toInnerRoom =
    (roomTopLeftCornerL %~ (\p -> p{x = x p + defaultThickness, y = y p + defaultThickness}))
        . (roomTopRightCornerL %~ (\p -> p{x = x p - defaultThickness, y = y p + defaultThickness}))
        . (roomBottomLeftCornerL %~ (\p -> p{x = x p + defaultThickness, y = y p - defaultThickness}))
        . (roomBottomRightCornerL %~ (\p -> p{x = x p - defaultThickness, y = y p - defaultThickness}))

toRectangle :: Room -> R.Rectangle
toRectangle r =
    R.Rectangle
        { R.topLeftCorner = topLeftCorner r
        , R.width = (x . topRightCorner) r - (x . topLeftCorner) r
        , R.height = (y . bottomLeftCorner) r - (y . topLeftCorner) r
        , R.fillColor = backgroundColor r
        , R.borderColor = Nothing
        }

instance Renderable Room where
    render r renderer gr = do
        R.drawRectangle renderer (toRectangle r)
        mapM_ (\w -> render w renderer gr) (view roomWallsL r)

standardRoom :: CInt -> CInt -> Room
standardRoom ww wh =
    Room
        { roomId = 0
        , topLeftCorner = Point{x = wwStep, y = whStep}
        , bottomLeftCorner = Point{x = wwStep, y = whStep * 6}
        , topRightCorner = Point{x = wwStep * 6, y = whStep}
        , bottomRightCorner = Point{x = wwStep * 6, y = whStep * 6}
        , backgroundColor = lightBrownColor
        }
  where
    wwStep = ww `div` 7
    whStep = wh `div` 7
