{-# LANGUAGE TupleSections #-}

module Init.GameResources (GameResources (..), cleanup, loadGameResources, gameResourceImagesTexturesL, gameResourcesGameResourceImagesL, gameResourceFontTTFsL, gameResourcesGameResourceFontsL, gameResourceMusicBackgroundMusicL, gameResourcesGameResourceMusicL, texturesLocations) where

import Control.Lens
import Data.Default.Class (def)
import Data.Map (Map, fromList, toList)
import Graphics.Texture (loadTexture)
import SDL.Font (Font, initialize)
import qualified SDL.Font as Font (load)
import SDL.Mixer (Chunk, free, haltMusic, load, openAudio, quit)
import SDL.Video (Renderer, Texture, destroyTexture)

newtype GameResourceMusic = GameResourceMusic
    { backgroundMusic :: Map FilePath Chunk
    }

newtype GameResourceImages = GameResourceImages
    { textures :: Map FilePath Texture
    }

newtype GameResourceFonts = GameResourceFonts
    { ttfs :: Map FilePath Font
    }

data GameResources = GameResources
    { music :: GameResourceMusic
    , images :: GameResourceImages
    , fonts :: GameResourceFonts
    }

-- resources location --------------------------------------

-- TODO: Get the filepaths from folders and the filesystem

texturesLocations :: [FilePath]
texturesLocations = ["./image/richard.png", "./image/menuBackground.png"]

backgroundMusicLocations :: [FilePath]
backgroundMusicLocations = ["./music/danzaMacabra.ogg"]

fontPointSize :: Int
fontPointSize = 24

fontsLocations :: [FilePath]
fontsLocations = ["./font/Impact.ttf"]

-- lenses --------------------------------------------------

gameResourcesGameResourceImagesL :: Lens' GameResources GameResourceImages
gameResourcesGameResourceImagesL = lens images (\gr gri -> gr{images = gri})

gameResourceImagesTexturesL :: Lens' GameResourceImages (Map FilePath Texture)
gameResourceImagesTexturesL = lens textures (\gri mts -> gri{textures = mts})

gameResourcesGameResourceMusicL :: Lens' GameResources GameResourceMusic
gameResourcesGameResourceMusicL = lens music (\gr grm -> gr{music = grm})

gameResourceMusicBackgroundMusicL :: Lens' GameResourceMusic (Map FilePath Chunk)
gameResourceMusicBackgroundMusicL = lens backgroundMusic (\grm mbk -> grm{backgroundMusic = mbk})

gameResourcesGameResourceFontsL :: Lens' GameResources GameResourceFonts
gameResourcesGameResourceFontsL = lens fonts (\gr grf -> gr{fonts = grf})

gameResourceFontTTFsL :: Lens' GameResourceFonts (Map FilePath Font)
gameResourceFontTTFsL = lens ttfs (\grf ts -> grf{ttfs = ts})

-- load ----------------------------------------------------

loadGameResources :: Renderer -> IO GameResources
loadGameResources r = do
    gameMusic <- loadGameMusic
    gameImages <- loadGameImages r
    gameFonts <- loadFonts
    return GameResources{music = gameMusic, images = gameImages, fonts = gameFonts}

loadGameMusic :: IO GameResourceMusic
loadGameMusic = do
    -- open device
    openAudio def 256
    mbkMusic <- traverse (\bkml -> (bkml,) <$> load bkml) backgroundMusicLocations
    haltMusic
    return $ GameResourceMusic{backgroundMusic = fromList mbkMusic}

loadGameImages :: Renderer -> IO GameResourceImages
loadGameImages r = do
    mts <- traverse (\tl -> (tl,) <$> loadTexture r tl) texturesLocations
    return $ GameResourceImages{textures = fromList mts}

loadFonts :: IO GameResourceFonts
loadFonts = do
    initialize
    fs <- traverse (\fl -> (fl,) <$> Font.load fl fontPointSize) fontsLocations
    return $ GameResourceFonts{ttfs = fromList fs}

-- cleanup -------------------------------------------------

cleanup :: GameResources -> IO ()
cleanup (GameResources{music = m, images = i}) = do
    cleanupMusic m
    cleanupImage i

cleanupMusic :: GameResourceMusic -> IO ()
cleanupMusic (GameResourceMusic{backgroundMusic = bms}) = do
    mapM_ free bms
    quit

cleanupImage :: GameResourceImages -> IO ()
cleanupImage gri =
    (mapM_ (destroyTexture . snd) . toList) $ view gameResourceImagesTexturesL gri
