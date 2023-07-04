module Init.GameSetup (GameSetup (..), withGameSetup) where

import Foreign.C.Types (CInt)
import qualified Graphics.Window as W (initializeWindow, windowSize)
import qualified Init.GameResources as GR (GameResources (..), cleanup, loadGameResources)
import SDL (initializeAll)
import SDL.Framerate (Manager, destroyManager, manager, set)
import qualified SDL.Init as Init (quit)
import SDL.Video (Renderer,rendererDrawBlendMode, BlendMode(..))
import Data.StateVar (($=))

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
    rendererDrawBlendMode r $= BlendAlphaBlend
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
