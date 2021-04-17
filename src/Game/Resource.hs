module Game.Resource where

import Apecs qualified
import Data.Map.Strict qualified as Map
import Optics.Core (Lens', set, view)
import Relude hiding (init)
import SDL qualified
import SDL.Font qualified as SDLFont
import SDL.Image qualified as SDLImage
import SDL.Mixer qualified as SDLMixer
import System.FilePath ((</>))
import World (System')
import World.Component (CResources, fonts, sounds, sprites)

lazyLoad :: Ord a => Lens' CResources (Map a b) -> System' b -> a -> System' b
lazyLoad optic initializeResource key = do
  resources <- Apecs.get Apecs.global
  let store = view optic resources
  case Map.lookup key store of
    Nothing -> do
      v <- initializeResource
      let updatedStore = Map.insert key v store
      Apecs.set Apecs.global (set optic updatedStore resources)
      pure v
    Just v -> pure v

resourceFont :: FilePath -> Int -> System' SDLFont.Font
resourceFont filepath fontSize =
  lazyLoad fonts initializeResource (filepath, fontSize)
  where
    initializeResource = SDLFont.load ("resources" </> filepath) fontSize

resourceSprite :: SDL.Renderer -> FilePath -> System' SDL.Texture
resourceSprite renderer filepath = do
  lazyLoad sprites initializeResource filepath
  where
    initializeResource = SDLImage.loadTexture renderer ("resources" </> filepath)

resourceSound :: FilePath -> System' SDLMixer.Chunk
resourceSound filepath = do
  lazyLoad sounds initializeResource filepath
  where
    initializeResource = SDLMixer.load ("resources" </> filepath)
