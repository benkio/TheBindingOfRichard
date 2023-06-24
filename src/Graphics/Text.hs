module Graphics.Text (Text (..)) where

import Control.Lens
import qualified Data.Map as M (lookup)
import Data.Text (pack)
import Foreign.C.Types (CInt)
import Graphics.Color (Color (..), colorToV4, whiteColor)
import Graphics.Point (Point (..))
import qualified Graphics.Rectangle as R (Rectangle (..))
import Graphics.Texture (renderTexture)
import Init.GameResources (gameResourceFontTTFsL, gameResourcesGameResourceFontsL)
import Render.Renderable (Renderable (..))
import SDL (createTextureFromSurface)
import qualified SDL.Font as Font (blended)
import Text.Printf
import Prelude hiding (lookup)

data Text = Text
    { value :: String
    , fontLocation :: String
    , width :: CInt
    , height :: CInt
    , color :: Color
    , position :: Point
    }
    deriving (Eq)

instance Renderable Text where
    render
        ( Text
                { value = v
                , fontLocation = fl
                , width = w
                , height = h
                , color = c
                , position = p
                }
            )
        renderer
        gr = do
            -- Render title
            textSurface <- maybe (error (printf "Text Font Location %s not found in Game Resources" fl)) (\font -> Font.blended font (colorToV4 whiteColor) (pack v)) mf
            titleTexture <- createTextureFromSurface renderer textSurface
            renderTexture renderer titleTexture $ R.Rectangle{R.topLeftCorner = p, R.width = w, R.height = h, R.fillColor = c, R.borderColor = Nothing}
          where
            mf = M.lookup fl $ view (gameResourcesGameResourceFontsL . gameResourceFontTTFsL) gr
