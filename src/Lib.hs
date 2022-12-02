{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib (run) where

import Control.Concurrent (threadDelay)
import Controls (defaultControls)
import GameState (GameState (..), initialGameState, transformGameState)
import Graphics.Point (pointToSDLPoint)
import Graphics.Window (initializeWindow, windowToBlack)
import SDL

run :: IO ()
run = do
  initializeAll
  (_, renderer) <- initializeWindow
  appLoop renderer initialGameState

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state = do
  events <- pollEvents
  let maybeNewState = transformGameState events defaultControls state
  maybe
    (return ())
    ( \newState -> do
        windowToBlack renderer
        rendererDrawColor renderer $= V4 0 0 255 255
        fillRect renderer (Just (Rectangle (pointToSDLPoint (position newState)) (V2 20 20)))
        present renderer
        threadDelay 30000
        appLoop renderer newState
    )
    maybeNewState
