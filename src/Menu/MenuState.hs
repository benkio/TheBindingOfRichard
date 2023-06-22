module Menu.MenuState (MenuState (..)) where

data MenuState = MenuState
    { selectedMenu :: Int
    , selectedOption :: Int
    }
