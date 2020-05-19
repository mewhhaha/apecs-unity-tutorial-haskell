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

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Word (Word8)
import Engine.SDL.Internal
import Polysemy
import Polysemy.Resource
import Polysemy.State
import qualified Rapid
import qualified SDL
import SDL (($=), V4)
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer
import Type.Reflection

data Rapid m a where
  CreateRef :: Typeable a => String -> IO a -> Rapid m a
  WriteRef :: Typeable a => String -> IO a -> Rapid m a
  UnlessRapid :: IO () -> Rapid m ()

makeSem ''Rapid

runRapid :: Member (Embed IO) r => Maybe (Rapid.Rapid String) -> Sem (Rapid : r) a -> Sem r a
runRapid mr =
  let runDev :: Member (Embed IO) r => (Rapid.Rapid String -> String -> IO a -> IO a) -> String -> IO a -> Sem r a
      runDev f s io = case mr of
        Nothing -> embed io
        Just r -> embed (f r s io)
   in interpret $ \case
        CreateRef s io -> runDev Rapid.createRef s io
        WriteRef s io -> runDev Rapid.writeRef s io
        UnlessRapid io -> runDev (\_ _ _ -> pure ()) "" io

data SDL m a where
  SetHintQuality :: SDL m ()
  PollEvents :: SDL m [SDL.Event]
  Time :: SDL m Double

makeSem ''SDL

runSDL :: Members [Resource, Embed IO] r => Sem (SDL : r) a -> Sem r a
runSDL =
  bracket
    ( do
        SDL.initialize [SDL.InitVideo, SDL.InitAudio]
        SDL.Image.initialize []
        SDL.Font.initialize
        SDL.Mixer.initialize []
        SDL.Mixer.openAudio SDL.Mixer.defaultAudio 256
    )
    ( const $ do
        SDL.Mixer.quit
        SDL.Font.quit
        SDL.Image.quit
        SDL.quit
    )
    . const
    . interpret
      ( \case
          SetHintQuality -> SDL.HintRenderScaleQuality $= SDL.ScaleNearest
          PollEvents -> SDL.pollEvents
          Time -> SDL.time
      )

data SDLWindow m a where
  GetWindow :: SDLWindow m SDL.Window

makeSem ''SDLWindow

runSDLWindow :: Members [Rapid, Resource, Embed IO, SDL] r => Text -> (Int, Int) -> Sem (SDLWindow : r) a -> Sem r a
runSDLWindow title (x, y) sem = bracket before after $ \w -> do
  SDL.showWindow w
  interpret
    ( \case
        GetWindow -> pure w
    )
    sem
  where
    before = createRef "win" (SDL.createWindow title p)
    after = unlessRapid . SDL.destroyWindow
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

runSDLRenderer :: Members [Rapid, Resource, Embed IO, SDLWindow] r => Sem (SDLRenderer : r) a -> Sem r a
runSDLRenderer sem = do
  w <- getWindow
  let before = createRef "renderer" (SDL.createRenderer w (-1) rendererConfig)
      after = unlessRapid . SDL.destroyRenderer
  bracket before after $ \r ->
    interpret
      ( \case
          GetRenderer -> pure r
      )
      sem

data SDLLoad m a where
  LoadTexture :: FilePath -> SDLLoad m Texture
  LoadAudio :: SDL.Mixer.Loadable a => FilePath -> SDLLoad m a
  LoadAudioRaw :: FilePath -> SDLLoad m ByteString
  LoadFont :: [SDL.Font.PointSize] -> FilePath -> SDLLoad m (Map.Map SDL.Font.PointSize SDL.Font.Font)

makeSem ''SDLLoad

runSDLLoad :: Members [SDL, SDLRenderer, Embed IO] r => Sem (SDLLoad : r) a -> Sem r a
runSDLLoad =
  interpret $
    \case
      LoadTexture fp -> do
        r <- getRenderer
        t <- SDL.Image.loadTexture r fp
        i <- SDL.queryTexture t
        pure $ mkTexture t i
      LoadFont sizes fp -> do
        fonts <- mapM (SDL.Font.load fp) sizes
        return . Map.fromList $ zip sizes fonts
      LoadAudio fp -> SDL.Mixer.load fp
      LoadAudioRaw fp -> embed (ByteString.readFile fp)

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
  GetEvents :: Loop m [SDL.Event]

makeSem ''Loop

runLoop :: (Typeable w, Members [Rapid, SDL, State Double, Embed IO] r) => w -> (w -> Sem (Loop : r) (Maybe w)) -> Sem r ()
runLoop a sem = do
  w <- writeRef "world" (pure a)
  curr <- time
  prev <- get @Double
  put curr
  events <- pollEvents
  result <-
    interpret
      ( \case
          DeltaTime -> return $ curr - prev
          GetEvents -> return events
      )
      (sem w)
  case result of
    Nothing -> return ()
    Just w' -> runLoop w' sem
