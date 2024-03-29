module Menu.Model.Menu (Menu (..), menuOptionIds, changeSelectedOption, getSelectedOptionId) where

import Control.Lens
import Control.Monad (unless)
import qualified Data.Map as M (lookup)
import Graphics.Color (whiteColor)
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..))
import Graphics.Text (Text)
import Graphics.Texture (renderTexture)
import qualified Graphics.Window as W (windowSize)
import Init.GameResources (
    gameResourceImagesTexturesL,
    gameResourceMusicBackgroundMusicL,
    gameResourcesGameResourceImagesL,
    gameResourcesGameResourceMusicL,
 )
import Menu.Model.MenuOption (MenuOption, menuOptionIds, select)
import qualified Menu.Model.MenuOption as MO (getSelectedOptionId)
import Render.Renderable (Renderable (..))
import qualified SDL.Mixer as Mix
import Text.Printf
import Prelude hiding (lookup)

data Menu = Menu
    { options :: [MenuOption]
    , title :: Text
    , menuBackgroundImageLocation :: String
    , menuBackgroundMusicLocation :: String
    }
    deriving (Eq, Show)

changeSelectedOption :: Menu -> Int -> Menu
changeSelectedOption m moid = m{options = (fmap (`select` moid) . options) m}

getSelectedOptionId :: Menu -> Int
getSelectedOptionId = MO.getSelectedOptionId . options

instance Renderable Menu where
    render
        ( Menu
                { options = ops
                , title = t
                , menuBackgroundImageLocation = bgi
                , menuBackgroundMusicLocation = bgm
                }
            )
        renderer
        gr = do
            -- Window Size and step
            (windowWidth, windowHeight) <- W.windowSize -- TODO: This should come from the gamesetup and we should not have to get it here
            -- Play background Music
            somethingPlaying <- Mix.playing Mix.AllChannels
            unless somethingPlaying $ maybe (error (printf "Main Menu Background Music Location %s not found in Game Resources" bgm)) Mix.play mbgm

            -- Render Background
            maybe
                (error (printf "Main Menu Background Texture Location %s not found in Game Resources" bgi))
                (\bgit -> renderTexture renderer bgit Rectangle{topLeftCorner = Point{x = 0, y = 0}, width = windowWidth, height = windowHeight, fillColor = whiteColor, borderColor = Nothing})
                mbgit

            -- Render title
            render t renderer gr
            -- Render Options
            mapM_ (\o -> render o renderer gr) ops
          where
            mbgit = M.lookup bgi $ view (gameResourcesGameResourceImagesL . gameResourceImagesTexturesL) gr
            mbgm = M.lookup bgm $ view (gameResourcesGameResourceMusicL . gameResourceMusicBackgroundMusicL) gr
