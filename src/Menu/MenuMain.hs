module Menu.MenuMain (run) where

import Control.Monad (when)
import Init.GameSetup (GameSetup (..), withGameSetup)
import Menu.MenuState (MenuState (..), initialMenu, transformMenuState)
import Render.Renderable
import SDL (pollEvents)
import SDL.Framerate (delay_)
import Settings.Controls (defaultControls)

run :: IO ()
run =
    withGameSetup
        ( \gameSetup -> do
            render (initialMenu (windowSize gameSetup)) (renderer gameSetup) (gameResources gameSetup)
            menuLoop gameSetup (initialMenu (windowSize gameSetup))
        )

menuLoop :: GameSetup -> MenuState -> IO ()
menuLoop gameSetup state = do
    evs <- pollEvents
    let maybeNewState = transformMenuState evs defaultControls state
    render (initialMenu (windowSize gameSetup)) (renderer gameSetup) (gameResources gameSetup)

    maybe
        (pure ())
        ( \newState -> do
            when (newState /= state) $ render newState (renderer gameSetup) (gameResources gameSetup)
            delay_ (framerateManager gameSetup)
            menuLoop gameSetup newState
        )
        maybeNewState
