{-# LANGUAGE TupleSections #-}

module Game.Init.GameResources (GameResources (..), cleanup, loadGameResources, gameResourceImagesTexturesL, gameResourcesGameResourceImagesL, gameResourceMusicBackgroundMusicL, gameResourcesGameResourceMusicL, texturesLocations) where

import Control.Lens
import Data.Default.Class (def)
import Data.Map (Map, fromList, toList)
import Graphics.Texture (loadTexture)
import SDL.Mixer (Chunk, free, haltMusic, load, openAudio, quit)
import SDL.Video (Renderer, Texture, destroyTexture)

newtype GameResourceMusic = GameResourceMusic
    { backgroundMusic :: Map FilePath Chunk
    }

newtype GameResourceImages = GameResourceImages
    { textures :: Map FilePath Texture
    }

data GameResources = GameResources
    { music :: GameResourceMusic
    , images :: GameResourceImages
    }

-- resources location --------------------------------------

texturesLocations :: [FilePath]
texturesLocations = ["./image/richard.png"]

backgroundMusicLocations :: [FilePath]
backgroundMusicLocations = ["./music/danzaMacabra.ogg"]

-- lenses --------------------------------------------------

gameResourcesGameResourceImagesL :: Lens' GameResources GameResourceImages
gameResourcesGameResourceImagesL = lens images (\gr gri -> gr{images = gri})

gameResourcesGameResourceMusicL :: Lens' GameResources GameResourceMusic
gameResourcesGameResourceMusicL = lens music (\gr grm -> gr{music = grm})

gameResourceImagesTexturesL :: Lens' GameResourceImages (Map FilePath Texture)
gameResourceImagesTexturesL = lens textures (\gri mts -> gri{textures = mts})

gameResourceMusicBackgroundMusicL :: Lens' GameResourceMusic (Map FilePath Chunk)
gameResourceMusicBackgroundMusicL = lens backgroundMusic (\grm mbk -> grm{backgroundMusic = mbk})

-- load ----------------------------------------------------

loadGameResources :: Renderer -> IO GameResources
loadGameResources r = do
    gameMusic <- loadGameMusic
    gameImages <- loadGameImages r
    return GameResources{music = gameMusic, images = gameImages}

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
    (mapM_ (destroyTexture . snd) . toList) $ view (gameResourceImagesTexturesL) gri
