module GameSetup (GameSetup (..), withGameSetup) where

import Data.Default.Class (def)
import Foreign.C.Types (CInt)
import qualified Graphics.Window as W (initializeWindow, windowSize)
import SDL (initializeAll)
import SDL.Framerate (Manager, destroyManager, manager)
import qualified SDL.Init as Init (quit)
import SDL.Mixer (Chunk, free, haltMusic, load, openAudio)
import qualified SDL.Mixer as Mix (quit)
import SDL.Video (Renderer)

data GameSetup = GameSetup
    { renderer :: Renderer
    , backgroundMusic :: Chunk
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

    m <- manager
    let gameSetup =
            GameSetup
                { renderer = r
                , backgroundMusic = music
                , framerateManager = m
                , windowSize = ws
                }
    result <- gameLoop gameSetup
    cleanup gameSetup
    return result

cleanup :: GameSetup -> IO ()
cleanup (GameSetup{backgroundMusic = music, framerateManager = m}) = do
    free music
    destroyManager m
    Mix.quit
    Init.quit
