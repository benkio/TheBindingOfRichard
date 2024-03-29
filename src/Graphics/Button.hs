module Graphics.Button (Button (..), buildButton) where

import Foreign.C.Types (CInt)
import Graphics.Color (Color (..), greenColor)
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import qualified Graphics.Text as T (Text (..))
import Render.Renderable (Renderable (..))

data Button = Button
    { rectangle :: Rectangle
    , text :: T.Text
    , selected :: Bool
    }
    deriving (Show, Eq)

buildButton :: Point -> (CInt, CInt) -> Color -> Color -> FilePath -> String -> Button
buildButton p (bw, bh) bc tc fl tv =
    Button
        { selected = False
        , rectangle =
            Rectangle
                { topLeftCorner = p
                , width = bw
                , height = bh
                , fillColor = bc
                , borderColor = Nothing
                }
        , text =
            T.Text
                { T.value = tv
                , T.fontLocation = fl
                , T.width = xStep * 6
                , T.height = yStep * 6
                , T.color = tc
                , T.position = p{x = tpx, y = tpy}
                }
        }
  where
    xStep = bw `div` 10
    yStep = bh `div` 10
    tpx = x p + (xStep * 2)
    tpy = y p + (yStep * 2)

-- TODO: Implement
instance Renderable Button where
    render b r gr = do
        drawRectangle r rect
        render (text b) r gr
      where
        r' = rectangle b
        rect = if selected b then r'{borderColor = Just greenColor} else r'
