module Menu.Model.MenuOption.MenuOptionButton (MenuOptionButton(..)) where

import Render.Renderable (Renderable (..))

import Graphics.Button (Button)

data MenuOptionButton = MenuOptionButton
    { menuOptionId :: Int
    , targetMenu :: Int
    , button :: Button
    }


instance Renderable MenuOptionButton where
  render m r gr =
    render (button m) r gr
