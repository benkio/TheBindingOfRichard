module GameSetup (GameSetup (..), GameSetupImage (..), GameSetupMusic (..), withGameSetup) where

import Data.Default.Class (def)
import Foreign.C.Types (CInt)
import Graphics.Texture (loadTexture)
import qualified Graphics.Window as W (initializeWindow, windowSize)
import SDL (Texture, initializeAll)
import SDL.Framerate (Manager, destroyManager, manager, set)
import qualified SDL.Init as Init (quit)
import SDL.Mixer (Chunk, free, haltMusic, load, openAudio)
import qualified SDL.Mixer as Mix (quit)
import SDL.Video (Renderer, destroyTexture)

data GameSetupMusic = GameSetupMusic
    { backgroundMusic :: [Chunk]
    }

data GameSetupImage = GameSetupImage
    { playerTexture :: Texture
    }

data GameSetup = GameSetup
    { renderer :: Renderer
    , music :: GameSetupMusic
    , image :: GameSetupImage
    , framerateManager :: Manager
    , windowSize :: (CInt, CInt)
    }

withGameSetup :: (GameSetup -> IO ()) -> IO ()
withGameSetup gameLoop = do
    initializeAll
    (_, r) <- W.initializeWindow
    ws <- W.windowSize

    -- open device
    openAudio def 256
    music <- load "./music/danzaMacabra.ogg"
    haltMusic

    -- load player texture
    pt <- loadTexture r "./image/richard.png"

    m <- manager
    set m 60
    let gameSetup =
            GameSetup
                { renderer = r
                , music = GameSetupMusic{backgroundMusic = [music]}
                , image = GameSetupImage{playerTexture = pt}
                , framerateManager = m
                , windowSize = ws
                }
    result <- gameLoop gameSetup
    cleanup gameSetup
    return result

cleanup :: GameSetup -> IO ()
cleanup (GameSetup{music = ms, framerateManager = m, image = is}) = do
    cleanupMusic ms
    cleanupImage is
    destroyManager m
    Init.quit

cleanupMusic :: GameSetupMusic -> IO ()
cleanupMusic (GameSetupMusic{backgroundMusic = bms}) = do
    mapM_ free bms
    Mix.quit

cleanupImage :: GameSetupImage -> IO ()
cleanupImage (GameSetupImage{playerTexture = pt}) = destroyTexture pt
