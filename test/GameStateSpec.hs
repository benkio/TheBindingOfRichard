module GameStateSpec (gameStateSpec) where

import GameState (GameState (..), initialGameState)
import Graphics.Point (Point (..))
import Test.HUnit

gameStateSpec :: Test
gameStateSpec =
    TestList
        [ TestLabel "initialState should correctly build the initial state" testInitialState
        ]

testInitialState :: Test
testInitialState =
    TestCase $
        assertEqual "Check expected game state construction" (initialGameState (100, 100)) (GameState{position = Point{x = 50, y = 50}})
