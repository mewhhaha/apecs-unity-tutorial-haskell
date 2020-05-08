{-# LANGUAGE OverloadedStrings #-}

module Engine.SDL
  ( play,
    render,
  )
where

import Apecs (System, runWith)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Map as Map
import Engine.SDL.Internal (Drawable, Texture (..), getFrame, getTexture, isQuitEvent, mkRect, setHintQuality, withRenderer, withSDL, withSDLFont, withSDLImage, withWindow)
import qualified SDL
import SDL (($=), V2 (..), V4 (..))
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer

windowSize :: Integral a => (a, a)
windowSize = (640, 480)

render :: (MonadIO m, Integral p, Drawable a) => SDL.Renderer -> V2 p -> a -> m ()
render r (V2 x y) s = renderTexture r t f (mkRect (fromIntegral x) (fromIntegral y) w h)
  where
    (Texture t ti) = getTexture s
    size = V2 (SDL.textureWidth ti) (SDL.textureHeight ti)
    f = getFrame s
    (V2 w h) = maybe size (\(SDL.Rectangle _ size') -> size') f

renderTexture ::
  (Integral a, MonadIO m) =>
  SDL.Renderer ->
  SDL.Texture ->
  Maybe (SDL.Rectangle a) ->
  SDL.Rectangle a ->
  m ()
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
  (env -> Double -> as -> System w ()) ->
  -- | Drawing function
  (env -> SDL.Window -> SDL.Renderer -> System w ()) ->
  -- | Change function
  (env -> System w w) ->
  IO ()
play world createEnv handleEvents stepSystem drawSystem changeSystem =
  withSDL
    . withSDLImage
    . withSDLFont
    . SDL.Mixer.withAudio SDL.Mixer.defaultAudio 256
    $ do
      setHintQuality
      withWindow "My game" windowSize $ \w ->
        withRenderer w $ \r -> do
          env <- createEnv r
          t <- SDL.time
          loop 0 world $
            \dt curr -> do
              events <- SDL.pollEvents
              let as = handleEvents env events
              next <-
                runWith
                  curr
                  $ do
                    stepSystem env dt as
                    SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
                    SDL.clear r
                    drawSystem env w r
                    SDL.present r
                    changeSystem env
              return (any isQuitEvent events, next)
