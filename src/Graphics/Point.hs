module Graphics.Point (Point (..), pointToSDLPoint) where

import Foreign.C.Types (CInt)
import qualified SDL.Vect as Sdl (Point (..), V2 (..))

data Point = Point
  { x :: CInt,
    y :: CInt
  }
  deriving (Show)

pointToSDLPoint :: Point -> Sdl.Point Sdl.V2 CInt
pointToSDLPoint Point {x = px, y = py} = Sdl.P $ Sdl.V2 px py
