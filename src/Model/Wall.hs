module Model.Wall (Wall (..), wallEndL, wallStartL, wallThicknessL) where

import Control.Lens
import Graphics.Point (Point (..))

data Wall = Wall
    { start :: Point
    , end :: Point
    , thickness :: Int
    }
    deriving (Show, Eq)

wallStartL :: Lens' Wall Point
wallStartL = lens start (\wall p -> wall{start = p})

wallEndL :: Lens' Wall Point
wallEndL = lens end (\wall p -> wall{end = p})

wallThicknessL :: Lens' Wall Int
wallThicknessL = lens thickness (\wall v -> wall{thickness = v})
