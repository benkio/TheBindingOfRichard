{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Concurrent (threadDelay)
import Control.Monad
import Controls (defaultControls)
import Data.Maybe (fromJust, isNothing)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Foreign.C.Types
import GameState (GameState (..), initialGameState, transformGameState)
import qualified Graphics.Point as P (Point (..), pointToSDLPoint)
import Graphics.Window (initializeWindow, windowToBlack)
import SDL
import System.Random

run :: IO ()
run = do
  initializeAll
  (window, renderer) <- initializeWindow
  appLoop renderer initialGameState

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state = do
  events <- pollEvents
  let maybeNewState = transformGameState events defaultControls state
  unless (isNothing maybeNewState) $ do
    let newState = fromJust maybeNewState
    windowToBlack renderer
    rendererDrawColor renderer $= V4 0 0 255 255
    fillRect renderer (Just (Rectangle (P.pointToSDLPoint (position newState)) (V2 20 20)))
    present renderer
    threadDelay 30000
    appLoop renderer newState
