module Menu.Menu (menu) where

import Foreign.C.Types (CInt)
import Graphics.Button (buildButton)
import Graphics.Color (blackColor, lightBrownColor, whiteColor)
import Graphics.Point (Point (..))
import Graphics.Text (Text (..))
import Menu.Model.Menu (Menu (..))
import Menu.Model.MenuOption (MenuOption (..))
import Menu.Model.MenuOption.MenuOptionButton (MenuOptionButton (..))

menu :: (CInt, CInt) -> Menu
menu (windowWidth, windowHeight) =
    Menu
        { menuId = 0
        , options =
            [ MO
                ( MenuOptionButton
                    { menuOptionId = 0
                    , targetMenu = 1
                    , button = buildButton (Point{x = wws * 10, y = whs * 20}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "New Game"
                    }
                ),
                MO
                ( MenuOptionButton
                    { menuOptionId = 1
                    , targetMenu = 2
                    , button = buildButton (Point{x = wws * 10, y = whs * 40}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "Load Game"
                    }
                ),
                MO
                ( MenuOptionButton
                    { menuOptionId = 2
                    , targetMenu = 3
                    , button = buildButton (Point{x = wws * 10, y = whs * 60}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "Settings"
                    }
                ),
                MO
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
  where
    wws = windowWidth `div` 100
    whs = windowHeight `div` 100
