module Render.Renderable (Renderable (..)) where

import GameResources (GameResources (..))

import SDL.Video (Renderer)

class Renderable a where
    render :: a -> Renderer -> GameResources -> IO ()
