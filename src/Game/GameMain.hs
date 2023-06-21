{-# LANGUAGE ScopedTypeVariables #-}

module Game.GameMain (run) where

import Control.Lens
import Control.Monad (unless)
import qualified Data.Map as M (toList)
import Game.GameState (GameState (..), transformGameState)
import Game.Init.GameResources (gameResourceMusicBackgroundMusicL, gameResourcesGameResourceMusicL)
import Game.Init.GameSetup (GameSetup (..), withGameSetup)
import Game.Level.Level1 (gameState)
import Render.Renderable
import SDL (pollEvents)
import SDL.Framerate (delay_)
import qualified SDL.Mixer as Mix
import Settings.Controls (defaultControls)

-- TODO: Look at the sdl2-gfx for loading the framerate correctly
run :: IO ()
run =
    withGameSetup
        ( \gameSetup -> do
            let state = gameState (windowSize gameSetup)
            render state (renderer gameSetup) (gameResources gameSetup)
            appLoop gameSetup state
        )

appLoop :: GameSetup -> GameState -> IO ()
appLoop gameSetup state = do
    events <- pollEvents
    somethingPlaying <- Mix.playing Mix.AllChannels
    unless somethingPlaying $ Mix.play ((snd . head . M.toList . view (gameResourcesGameResourceMusicL . gameResourceMusicBackgroundMusicL) . gameResources) gameSetup) -- TODO: make this random when have multiple background music
    let maybeNewState = transformGameState events defaultControls state

    -- TODO: if Nothing don't render the gamestate but loop, otherwise render. On GameExit return Unit
    maybe
        (pure ())
        ( \newState -> do
            if newState /= state then render newState (renderer gameSetup) (gameResources gameSetup) else pure ()
            delay_ (framerateManager gameSetup)
            appLoop gameSetup newState
        )
        maybeNewState
