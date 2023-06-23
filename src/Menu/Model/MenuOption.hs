module Menu.Model.MenuOption (MenuOption (..)) where

import Render.Renderable (Renderable (..))

import Menu.Model.MenuOption.MenuOptionButton (MenuOptionButton)

newtype MenuOption = MO MenuOptionButton

instance Renderable MenuOption where
    render (MO m) r gr =
        render m r gr
