module Menu.MenuState (MenuState (..), transformMenuState, initialMenu) where

import Graphics.Color (Color (..), greenColor)

import qualified Graphics.Rectangle as GR (Rectangle (..))
import SDL (present)

import Foreign.C.Types (CInt)
import Game.GameMain (run)
import Graphics.Button (Button (..), buildButton)
import Graphics.Color (blackColor, lightBrownColor, whiteColor)
import Graphics.Point (Point (..))
import qualified Graphics.Text as GT (Text (..))
import Graphics.Window (windowToBlack)
import Menu.Model.Menu (Menu (..), changeSelectedOption, getSelectedOptionId, menuOptionIds)
import Menu.Model.MenuOption (MenuOption (..), Panel (..))
import Menu.Model.MenuOption.MenuOptionButton (MenuOptionButton (..))
import Model.Event (Event (..), toEventDefaultControl)
import qualified Model.Move as M (Move (..))
import Render.Renderable (Renderable (..))
import qualified SDL as S (Event)
import Settings.Controls (Controls)

newtype MenuState = MenuState {menu :: Menu} deriving (Eq, Show)

type MenuStateResult = Either (IO ()) MenuState

transformMenuState'' :: MenuState -> Event -> MenuStateResult
transformMenuState'' (MenuState{menu = m}) (GE move)
    | move == M.Up && (soid - 1) `elem` (concatMap menuOptionIds ops) = Right $ MenuState{menu = changeSelectedOption m (soid - 1)}
    | move == M.Down && (soid + 1) `elem` (concatMap menuOptionIds ops) = Right $ MenuState{menu = changeSelectedOption m (soid + 1)}
    | otherwise = Right (MenuState{menu = m})
  where
    ops = options m
    soid = getSelectedOptionId m
transformMenuState'' _ Quit = Left $ pure ()
transformMenuState'' ms Interact = optionMenuHandler soid ms
  where
    soid = (getSelectedOptionId . menu) ms

transformMenuState' :: S.Event -> Controls -> MenuState -> MenuStateResult
transformMenuState' ev controls ms =
    case toEventDefaultControl ev controls of
        Just e -> transformMenuState'' ms e
        Nothing -> Right ms

transformMenuState :: [S.Event] -> Controls -> MenuState -> MenuStateResult
transformMenuState [] _ ms = Right ms
transformMenuState (ev : evs) controls ms = case r of
    Right ms' -> transformMenuState evs controls ms'
    _ -> r
  where
    r = transformMenuState' ev controls ms

-- TODO: lenses
initialMenu :: (CInt, CInt) -> MenuState
initialMenu (windowWidth, windowHeight) =
    MenuState
        { menu =
            Menu
                { options =
                    [ MOB
                        ( MenuOptionButton
                            { menuOptionId = 0
                            , targetMenu = 1
                            , button = (\b -> b{selected = True, rectangle = (rectangle b){GR.borderColor = Just greenColor}}) (buildButton (Point{x = wws * 10, y = whs * 20}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "New Game")
                            }
                        )
                    , MOB
                        ( MenuOptionButton
                            { menuOptionId = 1
                            , targetMenu = 2
                            , button = buildButton (Point{x = wws * 10, y = whs * 40}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "Load Game"
                            }
                        )
                    , MOB
                        ( MenuOptionButton
                            { menuOptionId = 2
                            , targetMenu = 3
                            , button = buildButton (Point{x = wws * 10, y = whs * 60}) (wws * 30, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "Settings"
                            }
                        )
                    , MOB
                        ( MenuOptionButton
                            { menuOptionId = 3
                            , targetMenu = 4
                            , button = buildButton (Point{x = wws * 40, y = whs * 80}) (wws * 20, whs * 10) lightBrownColor blackColor "./font/Impact.ttf" "Quit"
                            }
                        )
                    , MOP
                        ( Panel
                            { panelRectangle =
                                GR.Rectangle
                                    { GR.topLeftCorner = Point{x = wws * 50, y = whs * 20}
                                    , GR.width = wws * 50
                                    , GR.height = whs * 50
                                    , GR.fillColor = blackColor{alpha = 100}
                                    , GR.borderColor = Nothing
                                    }
                            , contents = []
                            }
                        )
                    ]
                , title =
                    GT.Text
                        { GT.value = "The Binding Of Richard"
                        , GT.fontLocation = "./font/Impact.ttf"
                        , GT.width = wws * 70
                        , GT.height = whs * 10
                        , GT.color = whiteColor
                        , GT.position = Point{x = wws * 20, y = whs * 5}
                        }
                , menuBackgroundImageLocation = "./image/menuBackground.png"
                , menuBackgroundMusicLocation = "./music/danzaMacabra.ogg"
                }
        }
  where
    wws = windowWidth `div` 100
    whs = windowHeight `div` 100

optionMenuHandler :: Int -> MenuState -> MenuStateResult
optionMenuHandler 0 _ = Left $ run
optionMenuHandler 3 _ = Left $ pure ()
optionMenuHandler _ ms = Right ms

instance Renderable MenuState where
    render (MenuState{menu = m}) renderer gr = do
        windowToBlack renderer
        render m renderer gr
        present renderer
