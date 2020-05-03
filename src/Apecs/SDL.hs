{-# LANGUAGE OverloadedStrings #-}

module Apecs.SDL
  ( play,
    renderSprite,
    renderText,
  )
where

import Apecs (System, ask, runWith)
import qualified Apecs.SDL.Internal as Internal
import Control.Monad.Extra (unless, when, whileM)
import Data.Functor.Compose
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Foreign.C.Types
import qualified SDL
import SDL (($=), V2 (..), V4 (..))
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer

windowSize :: Integral a => (a, a)
windowSize = (640, 480)

draw :: SDL.Renderer -> w -> (SDL.Renderer -> System w ()) -> IO ()
draw r w drawSystem = do
  SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
  SDL.clear r
  runWith w (drawSystem r)
  SDL.present r

renderSprite :: (Integral p, Internal.Sprite a) => SDL.Renderer -> Internal.Point p -> a -> IO ()
renderSprite r (V2 x y) s = renderTexture r t f (Internal.mkRect (fromIntegral x) (fromIntegral y) w h)
  where
    (Internal.Texture t ti) = Internal.getTexture s
    size = V2 (SDL.textureWidth ti) (SDL.textureHeight ti)
    f = Internal.getFrame s
    (V2 w h) = maybe size (\(SDL.Rectangle _ size') -> size') f

renderText :: (Integral a) => SDL.Renderer -> SDL.Font.Font -> Text -> Internal.Point a -> IO ()
renderText r font text (V2 x y) = do
  surface <- SDL.Font.blended font (V4 maxBound maxBound maxBound maxBound) text
  texture <- SDL.createTextureFromSurface r surface
  info <- SDL.queryTexture texture
  let tw = SDL.textureWidth info
      th = SDL.textureHeight info
  renderTexture r texture Nothing (Internal.mkRect (fromIntegral x - tw `div` 2) (fromIntegral y) tw th)

renderTexture ::
  (Integral a) =>
  SDL.Renderer ->
  SDL.Texture ->
  Maybe (SDL.Rectangle a) ->
  SDL.Rectangle a ->
  IO ()
renderTexture r t mask pos =
  SDL.copy r t (fmap fromIntegral <$> mask) (Just $ fromIntegral <$> pos)

loop :: Double -> a -> (Double -> a -> IO (Bool, a)) -> IO ()
loop prev a op = do
  time <- SDL.time
  (quit, a') <- op (time - prev) a
  if quit then pure () else loop time a' op

play ::
  w ->
  -- | Environment function
  (SDL.Renderer -> IO env) ->
  -- | Event handling function
  (env -> [SDL.Event] -> as) ->
  -- | Stepping function
  (env -> Double -> as -> System w w) ->
  -- | Drawing function
  (env -> SDL.Window -> SDL.Renderer -> System w ()) ->
  IO ()
play world createEnv handleEvents stepSystem drawSystem =
  Internal.withSDL
    . Internal.withSDLImage
    . Internal.withSDLFont
    . SDL.Mixer.withAudio SDL.Mixer.defaultAudio 256
    $ do
      Internal.setHintQuality
      Internal.withWindow "My game" windowSize $ \w ->
        Internal.withRenderer w $ \r -> do
          env <- createEnv r
          t <- SDL.time
          loop 0 world $
            \dt curr -> do
              events <- SDL.pollEvents
              let as = handleEvents env events
              next <-
                runWith
                  curr
                  (stepSystem env dt as)
              draw r next (drawSystem env w)
              return (any Internal.isQuitEvent events, next)
