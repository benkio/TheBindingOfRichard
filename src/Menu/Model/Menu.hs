module Menu.Model.Menu (Menu (..)) where

import Control.Monad (unless)

import Control.Lens
import qualified Data.Map as M (lookup)
import Data.Text (pack)
import Graphics.Window (windowToBlack)
import Init.GameResources (
    gameResourceFontTTFsL,
    gameResourceImagesTexturesL,
    gameResourceMusicBackgroundMusicL,
    gameResourcesGameResourceFontsL,
    gameResourcesGameResourceImagesL,
    gameResourcesGameResourceMusicL,
 )
import qualified SDL.Mixer as Mix

import Graphics.Color (colorToV4, whiteColor)
import Graphics.Point (Point (..))
import Graphics.Rectangle (Rectangle (..))
import Graphics.Texture (renderTexture)
import qualified Graphics.Window as W (windowSize)
import Menu.Model.MenuOption (MenuOption)
import Render.Renderable (Renderable (..))
import SDL (createTextureFromSurface, present)
import qualified SDL.Font as Font (blended)
import Text.Printf
import Prelude hiding (lookup)

data Menu = Menu
    { menuId :: Int
    , options :: [MenuOption]
    , title :: String
    , titleFontLocation :: String
    , menuBackgroundImageLocation :: String
    , menuBackgroundMusicLocation :: String
    }

instance Renderable Menu where
    render
        ( Menu
                { --options = ops,
                title = t
                , titleFontLocation = tfl
                , menuBackgroundImageLocation = bgi
                , menuBackgroundMusicLocation = bgm
                }
            )
        renderer
        gr = do
            -- Clean window
            windowToBlack renderer

            -- Window Size and step
            (windowWidth, windowHeight) <- W.windowSize -- TODO: This should come from the gamesetup and we should not have to get it here
            let wws = windowWidth `div` 100
                whs = windowWidth `div` 100
            -- Play background Music
            somethingPlaying <- Mix.playing Mix.AllChannels
            unless somethingPlaying $ maybe (error (printf "Main Menu Background Music Location %s not found in Game Resources" bgm)) Mix.play mbgm

            -- Render Background
            maybe
                (error (printf "Main Menu Background Texture Location %s not found in Game Resources" bgi))
                (\bgit -> renderTexture renderer bgit Rectangle{topLeftCorner = Point{x = 0, y = 0}, width = windowWidth, height = windowHeight, fillColor = whiteColor, borderColor = Nothing})
                mbgit

            -- Render title
            textSurface <- maybe (error (printf "Main Menu Font Location %s not found in Game Resources" tfl)) (\font -> Font.blended font (colorToV4 whiteColor) (pack t)) mf
            titleTexture <- createTextureFromSurface renderer textSurface
            renderTexture renderer titleTexture $ Rectangle{topLeftCorner = Point{x = wws * 20, y = whs * 5}, width = wws * 70, height = whs * 10, fillColor = whiteColor, borderColor = Nothing}
            -- TODO: start background music
            -- TODO: Render Options mapM_ (\l -> render l renderer gr) ls
            present renderer
          where
            mbgit = M.lookup bgi $ view (gameResourcesGameResourceImagesL . gameResourceImagesTexturesL) gr
            mbgm = M.lookup bgm $ view (gameResourcesGameResourceMusicL . gameResourceMusicBackgroundMusicL) gr
            mf = M.lookup tfl $ view (gameResourcesGameResourceFontsL . gameResourceFontTTFsL) gr
