module GameSetup (GameSetup (..), withGameSetup) where

import Foreign.C.Types (CInt)
import qualified GameResources as GR (GameResources (..), cleanup, loadGameResources)
import qualified Graphics.Window as W (initializeWindow, windowSize)
import SDL (initializeAll)
import SDL.Framerate (Manager, destroyManager, manager, set)
import qualified SDL.Init as Init (quit)
import SDL.Video (Renderer)

data GameSetup = GameSetup
    { renderer :: Renderer
    , framerateManager :: Manager
    , gameResources :: GR.GameResources
    , windowSize :: (CInt, CInt)
    }

withGameSetup :: (GameSetup -> IO ()) -> IO ()
withGameSetup gameLoop = do
    initializeAll
    (_, r) <- W.initializeWindow
    ws <- W.windowSize

    m <- manager
    set m 60
    gr <- GR.loadGameResources r
    let gameSetup =
            GameSetup
                { renderer = r
                , gameResources = gr
                , framerateManager = m
                , windowSize = ws
                }
    result <- gameLoop gameSetup
    cleanup gameSetup
    return result

cleanup :: GameSetup -> IO ()
cleanup (GameSetup{framerateManager = m, gameResources = gr}) = do
    GR.cleanup gr
    destroyManager m
    Init.quit
