{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (run) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Controls (defaultControls)
import Data.Maybe (fromMaybe, listToMaybe)
import GameState (GameState (..), gameStatePlayerL, initialGameState, transformGameState)
import Graphics.Window (initializeWindow, windowSize, windowToBlack)
import Render.Renderable
import SDL (Renderer, initializeAll, pollEvents, present)
import SDL.Video (Display (..), DisplayMode (..), getDisplays)

run :: IO ()
run = do
    initializeAll
    (_, renderer) <- initializeWindow
    igs <- initialGameState <$> windowSize
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
            render (view gameStatePlayerL newState) renderer
            present renderer
            threadDelay firstDisplayRefreshRate
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
