module Menu.MenuState (MenuState (..), transformMenuState, initialMenu) where

import SDL (present)

import Foreign.C.Types (CInt)
import Graphics.Button (buildButton)
import Graphics.Color (blackColor, lightBrownColor, whiteColor)
import Graphics.Point (Point (..))
import Graphics.Text (Text (..))
import Menu.Model.Menu (Menu (..), changeSelectedOption, getSelectedOptionId, menuOptionIds)
import Menu.Model.MenuOption (MenuOption (..), select)
import Menu.Model.MenuOption.MenuOptionButton (MenuOptionButton (..))
import Model.Event (Event (..), toEventDefaultControl)
import Model.Move (Move (..))
import Render.Renderable (Renderable (..))
import qualified SDL as S (Event)
import Settings.Controls (Controls)

newtype MenuState = MenuState {menu :: Menu} deriving (Eq)

transformMenuState'' :: MenuState -> Event -> Maybe MenuState
transformMenuState'' (MenuState{menu = m}) (GE move)
    | move == Up && soid `elem` (menuOptionIds m) = Just $ MenuState{menu = changeSelectedOption m (soid - 1)}
    | move == Down && soid `elem` (menuOptionIds m) = Just $ MenuState{menu = changeSelectedOption m (soid + 1)}
    | otherwise = Just (MenuState{menu = m})
  where
    soid = getSelectedOptionId m
transformMenuState'' _ Quit = Nothing
transformMenuState'' _ Interact = Nothing

transformMenuState' :: S.Event -> Controls -> MenuState -> Maybe MenuState
transformMenuState' ev controls ms =
    case toEventDefaultControl ev controls of
        Just e -> transformMenuState'' ms e
        Nothing -> Just ms

transformMenuState :: [S.Event] -> Controls -> MenuState -> Maybe MenuState
transformMenuState evs controls ms =
    foldl (\acc e -> acc >>= \st -> transformMenuState' e controls st) (Just ms) evs

initialMenu :: (CInt, CInt) -> MenuState
initialMenu (windowWidth, windowHeight) =
    MenuState
        { menu =
            Menu
                { options =
                    [ select
                        ( MO
                            ( MenuOptionButton
                                { menuOptionId = 0
                                , targetMenu = 1
                                , button = buildButton (Point{x = wws * 10, y = whs * 20}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "New Game"
                                }
                            )
                        )
                        True
                    , MO
                        ( MenuOptionButton
                            { menuOptionId = 1
                            , targetMenu = 2
                            , button = buildButton (Point{x = wws * 10, y = whs * 40}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "Load Game"
                            }
                        )
                    , MO
                        ( MenuOptionButton
                            { menuOptionId = 2
                            , targetMenu = 3
                            , button = buildButton (Point{x = wws * 10, y = whs * 60}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "Settings"
                            }
                        )
                    , MO
                        ( MenuOptionButton
                            { menuOptionId = 3
                            , targetMenu = 4
                            , button = buildButton (Point{x = wws * 40, y = whs * 80}) (wws * 20, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "Quit"
                            }
                        )
                    ]
                , title =
                    Text
                        { value = "The Binding Of Richard"
                        , fontLocation = "./font/Impact.ttf"
                        , width = wws * 70
                        , height = whs * 10
                        , color = whiteColor
                        , position = Point{x = wws * 20, y = whs * 5}
                        }
                , menuBackgroundImageLocation = "./image/menuBackground.png"
                , menuBackgroundMusicLocation = "./music/danzaMacabra.ogg"
                }
        }
  where
    wws = windowWidth `div` 100
    whs = windowHeight `div` 100

instance Renderable MenuState where
    render (MenuState{menu = m}) renderer gr = do
        render m renderer gr
        present renderer
