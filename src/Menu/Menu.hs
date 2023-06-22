module Menu.Menu (menu) where

import Foreign.C.Types (CInt)
import Graphics.Color (whiteColor)
import Graphics.Point (Point (..))
import Graphics.Text (Text (..))
import Menu.Model.Menu (Menu (..))

menu :: (CInt, CInt) -> Menu
menu (windowWidth, windowHeight) =
    Menu
        { menuId = 0
        , options = []
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
