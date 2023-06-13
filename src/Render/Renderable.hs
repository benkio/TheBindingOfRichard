module Render.Renderable (Renderable (..)) where

import SDL.Video (Renderer)

class Renderable a where
    render :: a -> Renderer -> IO ()
