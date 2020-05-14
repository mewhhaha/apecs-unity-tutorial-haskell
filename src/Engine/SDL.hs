{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Engine.SDL
  ( play,
    render,
    Engine,
    Environment,
  )
where

import Apecs (System, runWith)
import Control.Monad.IO.Class (MonadIO)
import Engine.SDL.Effect
import Engine.SDL.Internal
import Polysemy
import qualified SDL
import SDL (($=), V2 (..), V4 (..))

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

type Engine r = Members [Embed IO, SDL, SDLFont, SDLImage, SDLWindow, SDLRenderer, SDLMixer] r

loop :: Engine r => Double -> a -> (Double -> a -> Sem r (Bool, a)) -> Sem r ()
loop prev a op = do
  t <- time
  (quit, a') <- op (t - prev) a
  if quit then pure () else loop t a' op

type Environment r env = Members [SDLFont, SDLImage, SDLMixer] r => Sem r env

play ::
  Engine r =>
  w ->
  -- | Environment function
  Environment r env ->
  -- | Event handling function
  (env -> [SDL.Event] -> as) ->
  -- | Stepping function
  (env -> Double -> as -> System w ()) ->
  -- | Drawing function
  (env -> SDL.Window -> SDL.Renderer -> System w ()) ->
  -- | Change function
  (env -> System w w) ->
  Sem r ()
play world createEnv handleEvents stepSystem drawSystem changeSystem = do
  w <- getWindow
  r <- getRenderer
  setHintQuality
  env <- createEnv
  loop 0 world $
    \dt curr -> do
      events <- pollEvents
      let as = handleEvents env events
      next <-
        embed
          ( runWith
              curr
              $ do
                stepSystem env dt as
                SDL.rendererDrawColor r $= SDL.V4 maxBound maxBound maxBound maxBound
                SDL.clear r
                drawSystem env w r
                SDL.present r
                changeSystem env
          )
      return (any isQuitEvent events, next)
