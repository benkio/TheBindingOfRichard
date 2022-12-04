{-# LANGUAGE OverloadedStrings #-}

module Graphics.Window (initializeWindow, windowToBlack, windowHeight, windowWidth) where

import Foreign.C.Types (CInt)
import SDL (($=))
import SDL.Vect (V2 (..), V4 (..))
import SDL.Video (Renderer, Window, clear, createRenderer, createWindow, defaultWindow, rendererDrawColor, windowInitialSize)
import SDL.Video.Renderer (defaultRenderer)

-- TODO: Set these dinamically from the Display at index 0 https://hackage.haskell.org/package/sdl2-2.5.4.0/docs/SDL-Video.html#v:displayBoundsSize
windowHeight :: CInt
windowHeight = 720

windowWidth :: CInt
windowWidth = 1280

initializeWindow :: IO (Window, Renderer)
initializeWindow = do
  window <- createWindow "TheBindingOfRichard" defaultWindow {windowInitialSize = V2 windowWidth windowHeight}
  renderer <- createRenderer window (-1) defaultRenderer
  return (window, renderer)

windowToBlack :: Renderer -> IO ()
windowToBlack renderer = do
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
