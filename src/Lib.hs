{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Foreign.C.Types
import SDL
import System.Random

run :: IO ()
run = do
  gen <- initStdGen
  initializeAll
  window <- createWindow "Snek" defaultWindow {windowInitialSize = V2 1200 800}
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer $ initialGameState gen

data GameState = GameState
  { x :: CInt,
    y :: CInt,
    dx :: CInt,
    dy :: CInt,
    tailLength :: Int,
    tailElems :: Seq (V2 CInt),
    fruit :: V2 CInt,
    rndGen :: StdGen,
    speed :: Int
  }

initialGameState :: StdGen -> GameState
initialGameState gen =
  GameState
    { x = 4,
      y = 10,
      dx = 0,
      dy = 0,
      tailLength = 20,
      tailElems = mempty,
      fruit = V2 20 22,
      rndGen = gen,
      speed = 100000
    }

appLoop :: Renderer -> GameState -> IO ()
appLoop renderer state@GameState {..} = do
  events <- pollEvents
  let eventIsKeyPressed keyCode event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == keyCode
          _ -> False
      qPressed = any (eventIsKeyPressed KeycodeQ) events
      upBtnPressed = any (eventIsKeyPressed KeycodeW) events
      downBtnPressed = any (eventIsKeyPressed KeycodeS) events
      leftBtnPressed = any (eventIsKeyPressed KeycodeA) events
      rightBtnPressed = any (eventIsKeyPressed KeycodeD) events
  let (dx', dy') =
        case (compare dx 0, compare dy 0) of
          (EQ, EQ) | upBtnPressed -> (0, -1)
          (EQ, EQ) | downBtnPressed -> (0, 1)
          (EQ, EQ) | leftBtnPressed -> (-1, 0)
          (EQ, EQ) | rightBtnPressed -> (1, 0)
          (EQ, EQ) -> (0, 0)
          (LT, EQ) | upBtnPressed -> (0, -1)
          (LT, EQ) | downBtnPressed -> (0, 1)
          (LT, EQ) -> (dx, dy)
          (EQ, LT) | leftBtnPressed -> (-1, 0)
          (EQ, LT) | rightBtnPressed -> (1, 0)
          (EQ, LT) -> (dx, dy)
          (GT, EQ) | upBtnPressed -> (0, -1)
          (GT, EQ) | downBtnPressed -> (0, 1)
          (GT, EQ) -> (dx, dy)
          (EQ, GT) | leftBtnPressed -> (-1, 0)
          (EQ, GT) | rightBtnPressed -> (1, 0)
          (EQ, GT) -> (dx, dy)
          _ -> error "Impossible input state"
      x' = (x + dx') `mod` 60
      y' = (y + dy') `mod` 40
      tailElems' = case compare (S.length tailElems) tailLength of
        GT -> error "Impossible tail state!"
        EQ | tailLength > 0 -> S.drop 1 tailElems |> V2 x' y'
        EQ -> tailElems
        LT -> tailElems |> V2 x' y'
      (fruit', rndGen') =
        if fruit /= V2 x y
          then (fruit, rndGen)
          else
            let (fx, g') = uniformR (0, 59) rndGen
                (fy, g'') = uniformR (0, 39) g'
             in (V2 fx fy, g'')
      tailLength' = if fruit /= fruit' then tailLength + 1 else tailLength
      speed' = if tailLength' /= tailLength && speed >= 20000 then speed - 20000 else speed
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect renderer (Just (Rectangle (P $ (* 20) <$> fruit) (V2 20 20)))
  _ <- S.traverseWithIndex (renderTailElem renderer) tailElems
  rendererDrawColor renderer $= V4 0 0 255 255
  fillRect renderer (Just (Rectangle (P $ V2 (x' * 20) (y' * 20)) (V2 20 20)))
  present renderer
  unless qPressed $ do
    threadDelay 100000
    appLoop
      renderer
      state
        { x = x',
          y = y',
          dx = dx',
          dy = dy',
          tailElems = tailElems',
          fruit = fruit',
          tailLength = tailLength',
          rndGen = rndGen',
          speed = speed'
        }
  where
    renderTailElem :: Renderer -> Int -> V2 CInt -> IO ()
    renderTailElem r ix e = do
      let blueIx = (1 + ix) * 10
          blue = if blueIx <= 255 then blueIx else 255
      rendererDrawColor r $= V4 0 0 (fromIntegral blue) 255
      fillRect renderer (Just (Rectangle (P $ (* 20) <$> e) (V2 20 20)))
