module Menu.Model.MenuOption.MenuOptionButton (MenuOptionButton) where

data MenuOptionButton = MenuOptionButton
    { menuOptionId :: Int
    , targetMenu :: Int
    , text :: String
    }
