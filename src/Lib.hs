{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (run) where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Controls (defaultControls)
import Data.Default.Class (def)
import Data.Maybe (fromMaybe, listToMaybe)
import Game.Level1 (gameState)
import GameState (GameState (..), transformGameState)
import Graphics.Window (initializeWindow, windowSize)
import Render.Renderable
import SDL (Renderer, initializeAll, pollEvents)
import qualified SDL.Init as SDLInit
import qualified SDL.Mixer as Mix
import SDL.Video (Display (..), DisplayMode (..), getDisplays)

-- TODO: Look at the sdl2-gfx for loading the framerate correctly
run :: IO ()
run = do
    initializeAll

    (_, renderer) <- initializeWindow
    igs <- gameState <$> windowSize

    -- open device
    Mix.openAudio def 256
    music <- Mix.load "./music/danzaMacabra.ogg"
    Mix.haltMusic

    appLoop renderer igs music

appLoop :: Renderer -> GameState -> Mix.Chunk -> IO ()
appLoop renderer state music = do
    events <- pollEvents
    somethingPlaying <- Mix.playing Mix.AllChannels
    unless somethingPlaying $ Mix.play music
    firstDisplayRefreshRate <- firstDisplayRefreshRateOrDefault
    let maybeNewState = transformGameState events defaultControls state
    maybe
        (cleanup music)
        ( \newState -> do
            render newState renderer
            threadDelay firstDisplayRefreshRate
            appLoop renderer newState music
        )
        maybeNewState

defaultDisplayRefreshRate :: Int
defaultDisplayRefreshRate = 30000

firstDisplayRefreshRateOrDefault :: IO Int
firstDisplayRefreshRateOrDefault = do
    mayFirstDisplay <- fmap listToMaybe getDisplays
    let mayFirstDisplayRefreshRate = fmap (fromIntegral . displayModeRefreshRate) $ mayFirstDisplay >>= listToMaybe . displayModes
    return $ fromMaybe defaultDisplayRefreshRate mayFirstDisplayRefreshRate

cleanup :: Mix.Chunk -> IO ()
cleanup music = do
    Mix.free music
    Mix.quit
    SDLInit.quit
