module Graphics.Rectangle (Rectangle (..), drawRectangle, rectangleToSDLRectangle) where

import Foreign.C.Types (CInt)
import Graphics.Color (Color (..), colorToV4)
import Graphics.Point (Point (..), pointToSDLPoint)
import SDL (($=))
import qualified SDL.Vect as SdlVect (V2 (..))
import SDL.Video (Renderer, rendererDrawColor)
import qualified SDL.Video.Renderer as SdlVideo (Rectangle (..), drawLine, fillRect)

data Rectangle = Rectangle
    { topLeftCorner :: !Point
    , width :: !CInt
    , height :: !CInt
    , fillColor :: Color
    , borderColor :: Maybe Color
    }
    deriving (Show, Eq)

rectangleToSDLRectangle :: Rectangle -> SdlVideo.Rectangle CInt
rectangleToSDLRectangle Rectangle{topLeftCorner = tlc, height = h, width = w} =
    SdlVideo.Rectangle (pointToSDLPoint tlc) (SdlVect.V2 w h)

drawRectangle :: Renderer -> Rectangle -> IO ()
drawRectangle renderer rectangle = do
    rendererDrawColor renderer $= colorToV4 (fillColor rectangle)
    SdlVideo.fillRect renderer ((Just . rectangleToSDLRectangle) rectangle)
    drawRectangleBorders renderer rectangle

drawRectangleBorders :: Renderer -> Rectangle -> IO ()
drawRectangleBorders renderer (Rectangle{width = w, height = h, topLeftCorner = tlc, borderColor = (Just bc)}) = do
    let trc = tlc{x = x tlc + w}
        brc = tlc{x = x tlc + w, y = y tlc + h}
        blc = tlc{y = y tlc + h}
    rendererDrawColor renderer $= colorToV4 bc
    SdlVideo.drawLine renderer (pointToSDLPoint tlc) (pointToSDLPoint blc)
    SdlVideo.drawLine renderer (pointToSDLPoint tlc) (pointToSDLPoint trc)
    SdlVideo.drawLine renderer (pointToSDLPoint blc) (pointToSDLPoint brc)
    SdlVideo.drawLine renderer (pointToSDLPoint brc) (pointToSDLPoint trc)
drawRectangleBorders _ _ = pure ()
