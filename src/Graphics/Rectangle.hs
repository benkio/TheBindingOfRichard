module Graphics.Rectangle (Rectangle (..), rectangleToSDLRectangle, drawRectangle) where

import Foreign.C.Types (CInt)
import Graphics.Color (Color (..), colorToV4)
import Graphics.Point (Point (..), pointToSDLPoint)
import SDL (($=))
import qualified SDL.Vect as SdlVect (V2 (..))
import SDL.Video (Renderer, rendererDrawColor)
import qualified SDL.Video.Renderer as SdlVideo (Rectangle (..), fillRect)

data Rectangle = Rectangle
    { topLeftCorner :: !Point
    , width :: !CInt
    , height :: !CInt
    }
    deriving (Show)

rectangleToSDLRectangle :: Rectangle -> SdlVideo.Rectangle CInt
rectangleToSDLRectangle Rectangle{topLeftCorner = tlc, height = h, width = w} =
    SdlVideo.Rectangle (pointToSDLPoint tlc) (SdlVect.V2 w h)

drawRectangle :: Renderer -> Color -> Rectangle -> IO ()
drawRectangle renderer color rectangle = do
    rendererDrawColor renderer $= colorToV4 color
    SdlVideo.fillRect renderer ((Just . rectangleToSDLRectangle) rectangle)
