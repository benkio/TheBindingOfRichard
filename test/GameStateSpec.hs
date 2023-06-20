module GameStateSpec (gameStateSpec) where

import Control.Lens hiding (levels)
import Controls (defaultControls)
import Data.Foldable (traverse_)
import Game.Level1 (gameState)
import GameState (GameState (..), gameStatePlayerL, transformGameState)
import Graphics.Point (Point (..))
import Model.Player (playerPositionL, playerPositionPositionL)
import qualified SDL
import Test.HUnit
import TestOps (arrowEventMap, buildKeypressEvent, quitEventMap, testGameState)

gameStateSpec :: Test
gameStateSpec =
    TestList
        [ TestLabel "initialState should correctly build the initial state" testInitialState
        , TestLabel "transformGameState should return `Nothing` if the event is `Quit`" testTransformGameStateQuit
        , TestLabel "transformGameState should return `Just GameState` with the position properly updated if the event is an arrow event" testTransformGameState
        , TestLabel "transformGameState should return the same GameState if the move is illegal" testTransformGameStateIllegalMove
        ]

testInitialState :: Test
testInitialState =
    TestCase $
        assertEqual "Check expected game state construction" testGameState expectedGameState
  where
    expectedGameState = (\gs -> gs{levels = []}) $ gameState (100, 100)

testTransformGameStateQuit :: Test
testTransformGameStateQuit =
    TestCase $
        traverse_ (\(e, _, _) -> assertEqual "Check the quit case, expected Nothing" Nothing (transformGameState [e] defaultControls testGameState)) quitEventMap

testTransformGameState :: Test
testTransformGameState =
    TestCase $
        traverse_ (\(e, _, f) -> assertEqual "Check the arrow case, expected movement" ((Just . f) gs) (transformGameState [e] defaultControls gs)) arrowEventMap
  where
    gs = gameState (100, 100)

testTransformGameStateIllegalMove :: Test
testTransformGameStateIllegalMove =
    TestList
        [ TestCase
            ( assertEqual "Illegal move Left provided, return the same gamestate" (Just gs) (transformGameState [buildKeypressEvent SDL.KeycodeLeft] defaultControls gs)
            )
        , TestCase
            ( assertEqual "Illegal move Up provided, return the same gamestate" (Just gs) (transformGameState [buildKeypressEvent SDL.KeycodeUp] defaultControls gs)
            )
        , TestCase
            ( assertEqual "Illegal move Down provided, return the same gamestate" (Just gs') (transformGameState [buildKeypressEvent SDL.KeycodeDown] defaultControls gs')
            )
        , TestCase
            ( assertEqual "Illegal move Right provided, return the same gamestate" (Just gs') (transformGameState [buildKeypressEvent SDL.KeycodeRight] defaultControls gs')
            )
        ]
  where
    gs = set (gameStatePlayerL . playerPositionL . playerPositionPositionL) (Point{x = 25, y = 25}) $ gameState (100, 100)
    gs' = set (gameStatePlayerL . playerPositionL . playerPositionPositionL) (Point{x = 73, y = 73}) $ gameState (100, 100)
