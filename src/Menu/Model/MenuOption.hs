module Menu.Model.MenuOption (MenuOption (..), menuOptionId, select, isSelected, flatMenuOptions, Panel(..)) where

import Graphics.Button (Button (..))
import Graphics.Color (greenColor)
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import qualified Menu.Model.MenuOption.MenuOptionButton as MOB (MenuOptionButton (..))
import Render.Renderable (Renderable (..))

data MenuOption =
  MOB MOB.MenuOptionButton
  | MOP Panel
  deriving (Show, Eq)

data Panel = Panel
    { panelRectangle :: Rectangle
    , contents :: [MenuOption]
    } deriving (Show, Eq)

instance Renderable MenuOption where
    render (MOB m) r gr = render m r gr
    render (MOP p) r gr = do
      drawRectangle r (panelRectangle p)
      mapM_ (\o -> render o r gr) (contents p)

flatMenuOptions :: MenuOption -> [MenuOption]
flatMenuOptions (MOB o) = [MOB o]
flatMenuOptions (MOP p) = contents p

menuOptionId :: MenuOption -> Int
menuOptionId (MOB m) = MOB.menuOptionId m
menuOptionId (MOP _) = error "Can't get Panel option id. Need to flat the option structure before"

isSelected :: MenuOption -> Bool
isSelected (MOB m) = (selected . MOB.button) m
isSelected (MOP _) = error "Can't get Panel option id. Need to flat the option structure before"

-- TODO: lenses
select :: MenuOption -> Bool -> MenuOption
select (MOB m) v = MOB $ m{MOB.button = b{selected = v, rectangle = r{borderColor = if v then Just greenColor else Nothing}}}
  where
    b = MOB.button m
    r = rectangle b
select (MOP _) _ = error "Can't select a Panel. Need to flat the option structure before selecting"
