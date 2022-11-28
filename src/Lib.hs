{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import SDL
import System.Random

run :: IO ()
run = do
  initializeAll
  window <- createWindow "TheBindingOfRichard" defaultWindow {windowInitialSize = V2 1200 800}
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer initialGameState

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state = do
  events <- pollEvents
  let maybeNewState = foldl (\mst e -> mst >>= \st -> transformGameState e defaultControls st) (Just state) events
  unless (isNothing maybeNewState) $ do
    let newState = fromJust maybeNewState
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    rendererDrawColor renderer $= V4 0 0 255 255
    fillRect renderer (Just (Rectangle (P $ V2 ((x newState) * 20) ((y newState) * 20)) (V2 20 20)))
    present renderer
    threadDelay 100000
    appLoop renderer newState
