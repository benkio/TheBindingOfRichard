{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (run) where

import Control.Concurrent (threadDelay)
import Controls (defaultControls)
import Data.Maybe (fromMaybe, listToMaybe)
import GameState (GameState (..), initialGameState, transformGameState)
import Graphics.Color (Color (..))
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import Graphics.Window (initializeWindow, windowToBlack)
import SDL (Renderer, initializeAll, pollEvents, present)
import SDL.Video (Display (..), DisplayMode (..), getDisplays)

run :: IO ()
run = do
    initializeAll
    (_, renderer) <- initializeWindow
    igs <- initialGameState
    appLoop renderer igs

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state = do
    events <- pollEvents
    firstDisplayRefreshRate <- firstDisplayRefreshRateOrDefault
    let maybeNewState = transformGameState events defaultControls state
    maybe
        (return ())
        ( \newState -> do
            windowToBlack renderer
            drawRectangle renderer (Color{red = 0, green = 0, blue = 255, alpha = 255}) Rectangle{topLeftCorner = position newState, width = 20, height = 20}
            present renderer
            threadDelay firstDisplayRefreshRate -- TODO: Set this dinamically based on the display refresh rate. https://hackage.haskell.org/package/sdl2-2.5.4.0/docs/SDL-Video.html#v:displayModeRefreshRate
            appLoop renderer newState
        )
        maybeNewState

defaultDisplayRefreshRate :: Int
defaultDisplayRefreshRate = 30000

firstDisplayRefreshRateOrDefault :: IO Int
firstDisplayRefreshRateOrDefault = do
    mayFirstDisplay <- fmap listToMaybe getDisplays
    let mayFirstDisplayRefreshRate = fmap (fromIntegral . displayModeRefreshRate) $ mayFirstDisplay >>= listToMaybe . displayModes
    return $ fromMaybe defaultDisplayRefreshRate mayFirstDisplayRefreshRate
