module GameStateSpec (gameStateSpec) where

import Controls (defaultControls)

import Data.Foldable (traverse_)
import GameState (transformGameState)
import Game.Level1 (gameState)
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
        assertEqual "Check expected game state construction" (gameState (100, 100)) testGameState

testTransformGameStateQuit :: Test
testTransformGameStateQuit =
    TestCase $
        traverse_ (\(e, _, _) -> assertEqual "Check the quit case, expected Nothing" (transformGameState [e] defaultControls testGameState) Nothing) quitEventMap

testTransformGameState :: Test
testTransformGameState =
    TestCase $
        traverse_ (\(e, _, f) -> assertEqual "Check the quit case, expected Nothing" (transformGameState [e] defaultControls testGameState) ((Just . f) testGameState)) arrowEventMap
