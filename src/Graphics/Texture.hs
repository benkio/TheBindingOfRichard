module Graphics.Texture (loadTexture, renderTexture) where

import Graphics.Rectangle (Rectangle (..), rectangleToSDLRectangle)
import qualified SDL
import qualified SDL.Image as IMG

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer imagePath = do
    surface <- IMG.load imagePath
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    return texture

renderTexture :: SDL.Renderer -> SDL.Texture -> Rectangle -> IO ()
renderTexture renderer texture rectangle = do
    let destination = (Just . rectangleToSDLRectangle) rectangle
    SDL.copy renderer texture Nothing destination
