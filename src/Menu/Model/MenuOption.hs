module Menu.Model.MenuOption (MenuOption (..), menuOptionId, select, isSelected) where

import Graphics.Button (Button (..))
import qualified Menu.Model.MenuOption.MenuOptionButton as MOB (MenuOptionButton (..))
import Render.Renderable (Renderable (..))

newtype MenuOption = MO MOB.MenuOptionButton deriving (Eq)

instance Renderable MenuOption where
    render (MO m) r gr =
        render m r gr

menuOptionId :: MenuOption -> Int
menuOptionId (MO m) = MOB.menuOptionId m

isSelected :: MenuOption -> Bool
isSelected (MO m) = (selected . MOB.button) m

-- TODO: lenses
select :: MenuOption -> Bool -> MenuOption
select (MO m) v = MO $ m{MOB.button = (MOB.button m){selected = v}}
