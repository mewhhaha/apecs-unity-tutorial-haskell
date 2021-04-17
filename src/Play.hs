module Play where

import Foreign.C (CInt)
import Relude
import SDL qualified
import SDL.Font qualified as SDLFont
import SDL.Image qualified as SDLImage
import SDL.Mixer qualified as SDLMixer
import Types (Command (..), Step, Tick (..))

rendererConfig :: SDL.RendererConfig
rendererConfig =
  SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedVSyncRenderer,
      SDL.rendererTargetTexture = False
    }

windowConfig :: SDL.V2 CInt -> SDL.WindowConfig
windowConfig windowSize =
  SDL.defaultWindow
    { SDL.windowInitialSize = windowSize,
      SDL.windowResizable = True
    }

play :: (MonadIO m) => Step m a -> Text -> SDL.V2 CInt -> m a -> m ()
play step title windowSize createInitialState = do
  SDLImage.initialize []
  SDLFont.initialize
  SDLMixer.initialize []
  SDLMixer.openAudio SDLMixer.defaultAudio 256
  window <- SDL.createWindow title $ windowConfig windowSize
  SDL.showWindow window
  renderer <- SDL.createRenderer window (-1) rendererConfig
  SDL.HintRenderScaleQuality SDL.$= SDL.ScaleNearest

  let loop previousTime currentState = do
        currentTime <- SDL.time
        events <- SDL.pollEvents
        let dt = currentTime - previousTime
        SDL.rendererDrawColor renderer SDL.$= minBound
        SDL.clear renderer
        command <- runExceptT . runReaderT step $ Tick dt events currentState window renderer
        SDL.present renderer
        case command of
          Left end -> case end of
            Reset -> do
              initialState <- createInitialState
              loop 0 initialState
            End -> pass
          Right nextState ->
            loop currentTime nextState

  initialState <- createInitialState
  loop 0 initialState

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDLMixer.quit
  SDLFont.quit
  SDLImage.quit
  SDL.quit
