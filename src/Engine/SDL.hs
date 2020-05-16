{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Engine.SDL
  ( play,
    render,
    Environment,
  )
where

import Apecs (System, runWith)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Engine.SDL.Effect
import Engine.SDL.Internal
import Linear (V4 (..))
import Polysemy
import Polysemy.Reader
import Polysemy.State
import qualified SDL
import SDL (V2 (..))

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

type Environment env r = Members [SDLFont, SDLImage, SDLMixer] r

play ::
  forall w r env.
  (Engine r, Environment env r) =>
  w ->
  Sem r env ->
  ((env, [SDL.Event], SDL.Renderer, SDL.Window, Double) -> System w w) ->
  Sem r ()
play world createEnv game = do
  r <- getRenderer
  w <- getWindow
  setHintQuality
  env <- createEnv
  let loop :: Members [Loop, Draw, Embed IO] q => w -> Sem q (Maybe w)
      loop world' = do
        dt <- deltaTime
        es <- events
        clear $ V4 maxBound maxBound maxBound maxBound
        next <- embed (runWith world' (game (env, es, r, w, dt)))
        present
        return $ if any isQuitEvent es then Nothing else Just next
  void . runDraw . runState @Double 0 . runLoop world $ loop
