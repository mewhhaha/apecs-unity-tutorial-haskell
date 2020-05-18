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
    Game,
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

type Engine r = Members [Embed IO, SDL, SDLWindow, SDLRenderer] r

type Environment env r = Member SDLLoad r

type Game env r = Members [Embed IO, Reader env, Loop, SDLRenderer, SDLWindow] r

play ::
  forall env r w.
  (Engine r, Environment env r) =>
  w ->
  Sem r env ->
  (forall q. Game env q => w -> Sem q w) ->
  Sem r ()
play world createEnv game = do
  setHintQuality
  env <- createEnv
  let loop w = do
        es <- getEvents
        clear $ V4 maxBound maxBound maxBound maxBound
        next <- runReader env $ game w
        present
        return $ if any isQuitEvent es then Nothing else Just next
  void . runDraw . runState @Double 0 . runLoop world $ loop
