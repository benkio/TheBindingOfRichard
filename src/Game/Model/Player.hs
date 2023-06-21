module Game.Model.Player (Player (..), PlayerPosition (..), playerPositionL, playerPositionPositionL, playerPositionRoomIdL, playerPositionLevelIdL, playerSize) where

import Control.Lens
import qualified Data.Map as M (lookup)
import Foreign.C.Types (CInt)
import Game.Init.GameResources (gameResourceImagesTexturesL, gameResourcesGameResourceImagesL)
import Graphics.Color (blueColor)
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..))
import Graphics.Texture (renderTexture)
import Render.Renderable (Renderable (..))
import Text.Printf

data PlayerPosition = PlayerPosition
    { position :: Point
    , roomId :: Int
    , levelId :: Int
    }
    deriving (Show, Eq)

data Player = Player
    { playerPosition :: PlayerPosition
    , playerTextureLocation :: FilePath
    }
    deriving (Show, Eq)

playerSize :: (CInt, CInt)
playerSize = (20, 20)

playerPositionL :: Lens' Player PlayerPosition
playerPositionL = lens playerPosition (\player p -> player{playerPosition = p})

playerPositionPositionL :: Lens' PlayerPosition Point
playerPositionPositionL = lens position (\pp p -> pp{position = p})

playerPositionRoomIdL :: Lens' PlayerPosition Int
playerPositionRoomIdL = lens roomId (\pp i -> pp{roomId = i})

playerPositionLevelIdL :: Lens' PlayerPosition Int
playerPositionLevelIdL = lens levelId (\pp i -> pp{levelId = i})

instance Renderable Player where
    render
        ( Player
                { playerPosition = PlayerPosition{position = p}
                , playerTextureLocation = ptl
                }
            )
        renderer
        gr =
            maybe
                (error (printf "Player Texture Location %s not found in Game Resources" ptl))
                (\pt -> renderTexture renderer pt Rectangle{topLeftCorner = p, width = fst playerSize, height = snd playerSize, fillColor = blueColor, borderColor = Nothing})
                mpt
          where
            mpt = M.lookup ptl $ view (gameResourcesGameResourceImagesL . gameResourceImagesTexturesL) gr
