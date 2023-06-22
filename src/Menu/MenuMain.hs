module Menu.MenuMain (run) where

import Init.GameSetup (GameSetup (..), withGameSetup)
import Menu.Menu (menu)
import Menu.MenuState (MenuState (..))
import Render.Renderable
import SDL (pollEvents)
import SDL.Framerate (delay_)

run :: IO ()
run =
    withGameSetup
        ( \gameSetup -> do
            render menu (renderer gameSetup) (gameResources gameSetup)
            menuLoop gameSetup $ MenuState{selectedMenu = 0, selectedOption = 0}
        )

menuLoop :: GameSetup -> MenuState -> IO ()
menuLoop gameSetup state = do
    _ <- pollEvents
    render menu (renderer gameSetup) (gameResources gameSetup)
    delay_ (framerateManager gameSetup)
    menuLoop gameSetup state
