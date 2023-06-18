module GameStateSpec (gameStateSpec) where

import Control.Lens
import Controls (defaultControls)
import Data.Foldable (traverse_)
import Game.Level1 (gameState)
import GameState (gameStateLevelsL, transformGameState)
import Test.HUnit
import TestOps (arrowEventMap, quitEventMap, testGameState)

gameStateSpec :: Test
gameStateSpec =
    TestList
        [ TestLabel "initialState should correctly build the initial state" testInitialState
        , TestLabel "transformGameState should return `Nothing` if the event is `Quit`" testTransformGameStateQuit
        , TestLabel "transformGameState should return `Just GameState` with the position properly updated if the event is an arrow event" testTransformGameState
        ]

testInitialState :: Test
testInitialState =
    TestCase $
        assertEqual "Check expected game state construction" testGameState (set gameStateLevelsL [] (gameState (100, 100)))

testTransformGameStateQuit :: Test
testTransformGameStateQuit =
    TestCase $
        traverse_ (\(e, _, _) -> assertEqual "Check the quit case, expected Nothing" Nothing (transformGameState [e] defaultControls testGameState)) quitEventMap

testTransformGameState :: Test
testTransformGameState =
    TestCase $
        traverse_ (\(e, _, f) -> assertEqual "Check the quit case, expected Nothing" ((Just . f) testGameState) (transformGameState [e] defaultControls testGameState)) arrowEventMap
