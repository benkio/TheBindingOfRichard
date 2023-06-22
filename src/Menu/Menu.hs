module Menu.Menu (menu) where

import Menu.Model.Menu (Menu (..))

menu :: Menu
menu =
    Menu
        { menuId = 0
        , options = []
        , title = "The Binding Of Richard"
        , titleFontLocation = "./font/Impact.ttf"
        , menuBackgroundImageLocation = "./image/menuBackground.png"
        , menuBackgroundMusicLocation = "./music/danzaMacabra.ogg"
        }
