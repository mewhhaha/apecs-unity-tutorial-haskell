{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Engine.SDL.Effect where

import qualified Apecs
import Control.Monad.Identity
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word8)
import Engine.SDL.Internal
import Polysemy
import Polysemy.Resource
import Polysemy.State
import qualified SDL
import SDL (($=), V4)
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer

data SDL m a where
  SetHintQuality :: SDL m ()
  PollEvents :: SDL m [SDL.Event]
  Time :: SDL m Double

makeSem ''SDL

runSDL :: Members [Resource, Embed IO] r => Sem (SDL : r) a -> Sem r a
runSDL =
  bracket (SDL.initialize [SDL.InitVideo, SDL.InitAudio]) (pure SDL.quit) . const
    . interpret
      ( \case
          SetHintQuality -> SDL.HintRenderScaleQuality $= SDL.ScaleNearest
          PollEvents -> SDL.pollEvents
          Time -> SDL.time
      )

data SDLWindow m a where
  GetWindow :: SDLWindow m SDL.Window

makeSem ''SDLWindow

runSDLWindow :: Members [Resource, Embed IO, SDL] r => Text -> (Int, Int) -> Sem (SDLWindow : r) a -> Sem r a
runSDLWindow title (x, y) sem = bracket before after $ \w -> do
  SDL.showWindow w
  interpret
    ( \case
        GetWindow -> pure w
    )
    sem
  where
    before = SDL.createWindow title p
    after = SDL.destroyWindow
    p = SDL.defaultWindow {SDL.windowInitialSize = z}
    z = SDL.V2 (fromIntegral x) (fromIntegral y)

data SDLRenderer m a where
  GetRenderer :: SDLRenderer m SDL.Renderer

makeSem ''SDLRenderer

rendererConfig :: SDL.RendererConfig
rendererConfig =
  SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedVSyncRenderer,
      SDL.rendererTargetTexture = False
    }

runSDLRenderer :: Members [Resource, Embed IO, SDLWindow] r => Sem (SDLRenderer : r) a -> Sem r a
runSDLRenderer sem = do
  w <- getWindow
  bracket (SDL.createRenderer w (-1) rendererConfig) SDL.destroyRenderer $ \r ->
    interpret
      ( \case
          GetRenderer -> pure r
      )
      sem

data SDLImage m a where
  LoadTexture :: FilePath -> SDLImage m Texture

makeSem ''SDLImage

runSDLImage :: Members [Resource, Embed IO, SDL, SDLRenderer] r => Sem (SDLImage : r) a -> Sem r a
runSDLImage =
  bracket (SDL.Image.initialize []) (pure SDL.Image.quit) . const
    . interpret
      ( \case
          LoadTexture fp -> do
            r <- getRenderer
            t <- SDL.Image.loadTexture r fp
            i <- SDL.queryTexture t
            pure $ mkTexture t i
      )

data SDLFont m a where
  LoadFont :: [SDL.Font.PointSize] -> FilePath -> SDLFont m (Map.Map SDL.Font.PointSize SDL.Font.Font)

makeSem ''SDLFont

runSDLFont :: Members [Resource, Embed IO, SDL] r => Sem (SDLFont : r) a -> Sem r a
runSDLFont =
  bracket SDL.Font.initialize (pure SDL.Font.quit) . const
    . interpret
      ( \case
          LoadFont sizes fp -> do
            fonts <- mapM (SDL.Font.load fp) sizes
            return . Map.fromList $ zip sizes fonts
      )

data SDLMixer m a where
  LoadAudio :: SDL.Mixer.Loadable a => FilePath -> SDLMixer m a
  LoadAudioRaw :: FilePath -> SDLMixer m ByteString

makeSem ''SDLMixer

runSDLMixer :: Members [Resource, Embed IO, SDL] r => Sem (SDLMixer : r) a -> Sem r a
runSDLMixer sem = bracket (SDL.Mixer.initialize []) (pure SDL.Mixer.quit) . const $ do
  SDL.Mixer.openAudio SDL.Mixer.defaultAudio 256
  interpret
    ( \case
        LoadAudio fp -> SDL.Mixer.load fp
        LoadAudioRaw fp -> embed (ByteString.readFile fp)
    )
    sem

data Draw m a where
  Clear :: V4 Word8 -> Draw m ()
  Present :: Draw m ()

makeSem ''Draw

runDraw :: Members [SDLRenderer, SDL, Embed IO] r => Sem (Draw : r) a -> Sem r a
runDraw sem = do
  r <- getRenderer
  interpret
    ( \case
        Clear color -> do
          SDL.rendererDrawColor r $= color
          SDL.clear r
        Present -> SDL.present r
    )
    sem

data Loop m a where
  DeltaTime :: Loop m Double
  Events :: Loop m [SDL.Event]

makeSem ''Loop

runLoop :: Members [SDL, State Double, Embed IO] r => w -> (w -> Sem (Loop : r) (Maybe w)) -> Sem r ()
runLoop a sem = do
  curr <- time
  prev <- get @Double
  put curr
  events <- pollEvents
  result <-
    interpret
      ( \case
          DeltaTime -> return $ curr - prev
          Events -> return events
      )
      (sem a)
  case result of
    Nothing -> return ()
    Just a' -> runLoop a' sem
