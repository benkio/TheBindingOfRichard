module Graphics.Color (Color (..), colorToV4, blueColor, greyColor, lightBrownColor, blackColor, whiteColor, greenColor) where

import Data.Word (Word8)
import SDL.Vect (V4 (..))

data Color = Color
    { red :: !Word8
    , green :: !Word8
    , blue :: !Word8
    , alpha :: !Word8
    }
    deriving (Show, Eq)

colorToV4 :: Color -> V4 Word8
colorToV4 c = V4 (red c) (green c) (blue c) (alpha c)

lightBrownColor :: Color
lightBrownColor = Color{red = 200, green = 160, blue = 130, alpha = 255}
blueColor :: Color
blueColor = Color{red = 0, green = 0, blue = 255, alpha = 255}
greenColor :: Color
greenColor = Color{red = 0, green = 255, blue = 0, alpha = 255}
greyColor :: Color
greyColor = Color{red = 200, green = 200, blue = 200, alpha = 255}
blackColor :: Color
blackColor = Color{red = 0, green = 0, blue = 0, alpha = 255}
whiteColor :: Color
whiteColor = Color{red = 255, green = 255, blue = 255, alpha = 255}
