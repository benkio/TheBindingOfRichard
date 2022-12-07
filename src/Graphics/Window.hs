{-# LANGUAGE OverloadedStrings #-}

module Graphics.Window (initializeWindow, windowToBlack, windowSize) where

import Data.Maybe (listToMaybe)
import Foreign.C.Types (CInt)
import SDL (($=))
import SDL.Vect (V2 (..), V4 (..))
import SDL.Video (Renderer, Window, clear, createRenderer, createWindow, defaultWindow, displayBoundsSize, getDisplays, rendererDrawColor, windowInitialSize)
import SDL.Video.Renderer (defaultRenderer)

--import Control.Exception (throwIO)

-- TODO: Set these dinamically from the Display at index 0 https://hackage.haskell.org/package/sdl2-2.5.4.0/docs/SDL-Video.html#v:displayBoundsSize
-- windowHeight :: CInt
-- windowHeight = 720

-- windowWidth :: CInt
-- windowWidth = 1280

windowSize :: IO (CInt, CInt)
windowSize = do
  displays <- getDisplays
  display <- maybe (fail "Display Not Found") pure $ listToMaybe displays
  let (V2 x y) = displayBoundsSize display
  return (x, y)

initializeWindow :: IO (Window, Renderer)
initializeWindow = do
  (windowWidth, windowHeight) <- windowSize
  window <- createWindow "TheBindingOfRichard" defaultWindow {windowInitialSize = V2 windowWidth windowHeight}
  renderer <- createRenderer window (-1) defaultRenderer
  return (window, renderer)

windowToBlack :: Renderer -> IO ()
windowToBlack renderer = do
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
