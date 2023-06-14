{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (run) where

import Control.Concurrent (threadDelay)
import Controls (defaultControls)
import Data.Maybe (fromMaybe, listToMaybe)
import Game.Level1 (gameState)
import GameState (GameState (..), transformGameState)
import Graphics.Window (initializeWindow, windowSize)
import Render.Renderable
import SDL (Renderer, initializeAll, pollEvents)
import SDL.Video (Display (..), DisplayMode (..), getDisplays)

run :: IO ()
run = do
    initializeAll
    (_, renderer) <- initializeWindow
    igs <- gameState <$> windowSize
    appLoop renderer igs

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state = do
    events <- pollEvents
    firstDisplayRefreshRate <- firstDisplayRefreshRateOrDefault
    let maybeNewState = transformGameState events defaultControls state
    maybe
        (return ())
        ( \newState -> do
            render newState renderer
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
