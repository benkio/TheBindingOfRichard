module Menu.Model.MenuOption (MenuOption (..), menuOptionIds, select, isSelected, Panel (..), getSelectedOptionId) where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Graphics.Button (Button (..))
import Graphics.Color (greenColor)
import Graphics.Rectangle (Rectangle (..), drawRectangle)
import qualified Menu.Model.MenuOption.MenuOptionButton as MOB (MenuOptionButton (..))
import Render.Renderable (Renderable (..))

data MenuOption
    = MOB MOB.MenuOptionButton
    | MOP Panel
    deriving (Show, Eq)

data Panel = Panel
    { panelRectangle :: Rectangle
    , contents :: [MenuOption]
    }
    deriving (Show, Eq)

instance Renderable MenuOption where
    render (MOB m) r gr = render m r gr
    render (MOP p) r gr = do
        drawRectangle r (panelRectangle p)
        mapM_ (\o -> render o r gr) (contents p)

menuOptionIds :: MenuOption -> [Int]
menuOptionIds (MOB m) = [MOB.menuOptionId m]
menuOptionIds (MOP (Panel{contents = cs})) = concatMap menuOptionIds cs

isSelected :: MenuOption -> Bool
isSelected (MOB m) = (selected . MOB.button) m
isSelected (MOP (Panel{contents = cs})) = any isSelected cs

getSelectedOptionId :: [MenuOption] -> Int
getSelectedOptionId = fromMaybe 0 . fmap getSelectedOptionId' . find isSelected
  where
    getSelectedOptionId' :: MenuOption -> Int
    getSelectedOptionId' (MOB m) = MOB.menuOptionId m
    getSelectedOptionId' (MOP p) = getSelectedOptionId (contents p)

-- TODO: lenses
select :: MenuOption -> Int -> MenuOption
select (MOB m) i
    | i `elem` (menuOptionIds (MOB m)) = MOB $ m{MOB.button = b{selected = True, rectangle = r{borderColor = Just greenColor}}}
    | otherwise = MOB $ m{MOB.button = b{selected = False, rectangle = r{borderColor = Nothing}}}
  where
    b = MOB.button m
    r = rectangle b
select (MOP p) i =
    (MOP (p{contents = fmap (\c -> select c i) (contents p)}))
