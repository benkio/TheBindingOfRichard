module Graphics.Color (Color (..), colorToV4) where

import Data.Word (Word8)
import SDL.Vect (V4 (..))

data Color = Color
  { red :: !Word8,
    green :: !Word8,
    blue :: !Word8,
    alpha :: !Word8
  }

colorToV4 :: Color -> V4 Word8
colorToV4 c = V4 (red c) (green c) (blue c) (alpha c)
