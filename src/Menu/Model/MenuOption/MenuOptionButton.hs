module Menu.Model.MenuOption.MenuOptionButton (MenuOptionButton) where

import Graphics.Button (Button)

data MenuOptionButton = MenuOptionButton
    { menuOptionId :: Int
    , targetMenu :: Int
    , button :: Button
    }
