module Graphics.Button (Button (..), buildButton) where

import Foreign.C.Types (CInt)
import Graphics.Color (Color (..))
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..))
import qualified Graphics.Text as T (Text (..))
import Render.Renderable (Renderable (..))

data Button = Button
    { rectangle :: Rectangle
    , text :: T.Text
    }

buildButton :: Point -> (CInt, CInt) -> Color -> Color -> FilePath -> String -> Button
buildButton p (bw, bh) bc tc fl tv =
    Button
        { rectangle =
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
                , T.width = ws * 8
                , T.height = hs * 8
                , T.color = tc
                , T.position = p{x = x p + ws, y = y p + hs}
                }
        }
  where
    ws = ((x p) `div` 10)
    hs = ((y p) `div` 10)

instance Renderable Button where
    render b r gr = undefined
