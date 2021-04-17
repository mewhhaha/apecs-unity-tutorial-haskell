module SDL.Draw where

import Relude
import SDL qualified

drawSprite :: (MonadIO m, Integral a1, Integral a2) => SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle a1) -> SDL.Rectangle a2 -> m ()
drawSprite r t mask pos = SDL.copy r t (fmap fromIntegral <$> mask) (Just $ fromIntegral <$> pos)