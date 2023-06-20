{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (run) where

import Control.Monad (unless)
import Controls (defaultControls)
import Game.Level1 (gameState)
import GameSetup (GameSetup (..), withGameSetup)
import GameState (GameState (..), transformGameState)
import Render.Renderable
import SDL (pollEvents)
import SDL.Framerate (delay_)
import qualified SDL.Mixer as Mix

-- TODO: Look at the sdl2-gfx for loading the framerate correctly
run :: IO ()
run =
    withGameSetup
        ( \gameSetup ->
            appLoop gameSetup $ gameState (windowSize gameSetup)
        )

appLoop :: GameSetup -> GameState -> IO ()
appLoop gameSetup state = do
    events <- pollEvents
    somethingPlaying <- Mix.playing Mix.AllChannels
    unless somethingPlaying $ Mix.play (backgroundMusic gameSetup)
    let maybeNewState = transformGameState events defaultControls state

    maybe
        (pure ())
        ( \newState -> do
            render newState (renderer gameSetup)
            delay_ (framerateManager gameSetup)
            appLoop gameSetup newState
        )
        maybeNewState
