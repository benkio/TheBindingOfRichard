{-# LANGUAGE ScopedTypeVariables #-}

module Game.GameMain (run) where

import Control.Lens
import Control.Monad (unless, when)
import qualified Data.Map as M (toList)
import Game.GameState (GameState (..), transformGameState)
import Game.Level.Level1 (gameState)
import Init.GameResources (gameResourceMusicBackgroundMusicL, gameResourcesGameResourceMusicL)
import Init.GameSetup (GameSetup (..), withGameSetup)
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
    unless somethingPlaying $ Mix.play ((snd . head . M.toList . view (gameResourcesGameResourceMusicL . gameResourceMusicBackgroundMusicL) . gameResources) gameSetup) -- TODO: make this random when have multiple background music. move the background music to the selected level and start it on render!
    let maybeNewState = transformGameState events defaultControls state

    maybe
        (pure ())
        ( \newState -> do
            when (newState /= state) $ render newState (renderer gameSetup) (gameResources gameSetup)
            delay_ (framerateManager gameSetup)
            appLoop gameSetup newState
        )
        maybeNewState
