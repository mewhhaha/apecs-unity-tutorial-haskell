{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Apecs.SDL.Internal where

import Control.Arrow ((&&&))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Fixed
import Data.Map.Strict as Map
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Data.Type.Equality ((:~:) (Refl))
import Foreign.C.Types
import GHC.Natural
import GHC.TypeLits.Compare (isLE)
import GHC.TypeNats
import Language.Haskell.TH
import Linear (V2 (..), V4 (..))
import qualified SDL
import SDL (($=))
import qualified SDL.Font
import qualified SDL.Image
import qualified SDL.Mixer

data Texture = Texture SDL.Texture SDL.TextureInfo

class Drawable a where
  getTexture :: a -> Texture
  getFrame :: a -> Maybe (SDL.Rectangle CInt)

newtype TextElement = TextElement Texture

type Size (n :: Nat) = Proxy n

type Frame (n :: Nat) = Proxy n

type Point a = V2 a

data Sheet a = Sheet {clip :: Maybe a, texture :: Texture, clips :: Map.Map a (SDL.Rectangle CInt)}

data ASheet (n :: Nat) where
  ASheet :: (KnownNat a, a <= n, 1 <= a, 1 <= n) => {frame :: Frame a, sheet :: Sheet Natural} -> ASheet n

instance Drawable Texture where
  getFrame _ = Nothing
  getTexture = id

instance Drawable TextElement where
  getFrame _ = Nothing
  getTexture (TextElement t) = t

instance (Ord a, Eq a) => Drawable (Sheet a) where
  getFrame Sheet {clip, clips} = clip >>= (`Map.lookup` clips)
  getTexture = texture

instance Drawable (ASheet a) where
  getFrame ASheet {frame, sheet} = getFrame (sheet {clip = Just n})
    where
      n = natVal frame
  getTexture = texture . sheet

mkTextElement :: MonadIO m => SDL.Renderer -> SDL.Font.Font -> Text -> m TextElement
mkTextElement r font text = do
  surface <- SDL.Font.blended font (V4 maxBound maxBound maxBound maxBound) text
  texture <- SDL.createTextureFromSurface r surface
  info <- SDL.queryTexture texture
  return $ TextElement (mkTexture texture info)

mkTexture :: SDL.Texture -> SDL.TextureInfo -> Texture
mkTexture = Texture

mkSheet :: Texture -> Map.Map a (SDL.Rectangle CInt) -> Sheet a
mkSheet = Sheet Nothing

mkASheet :: forall n i. (Integral i, KnownNat n, 1 <= n) => (i, i) -> (i, i) -> Texture -> ASheet n
mkASheet (w, h) (x, y) t@(Texture _ ti) = ASheet (Proxy @1) (mkSheet t clips)
  where
    size :: Natural
    size = natVal (Proxy :: Proxy n)
    tw :: i
    tw = fromIntegral $ SDL.textureWidth ti
    clips :: Map.Map Natural (SDL.Rectangle CInt)
    clips = Map.fromList $ rect <$> [1 .. size]
      where
        rect n =
          let k = fromIntegral n - 1
              offset = x + w * k
              x' = offset `mod` tw
              y' = y + (offset `div` tw) * h
           in (n, fromIntegral <$> mkRect x' y' w h)

animate :: forall b n. (KnownNat b, KnownNat n, 1 <= n, b <= n, 1 <= b) => ASheet n -> ASheet n
animate (ASheet _ sheet) = ASheet f sheet
  where
    f = Proxy :: Frame b

linear :: forall n. (KnownNat n, 1 <= n) => Double -> Double -> ASheet n -> ASheet n
linear time duration = case nat of
  (SomeNat p) -> case (isLE (Proxy @1) p, isLE p (Proxy @n)) of
    (Just Refl, Just Refl) -> decide p
    (Just Refl, Nothing) -> animate @n
    (Nothing, Just Refl) -> animate @1
  where
    size :: Natural
    size = natVal (Proxy @n)
    norm :: Double
    norm = if duration == 0 then 0 else (time `mod'` duration) / duration
    nat :: SomeNat
    nat = someNatVal (floor (fromIntegral size * norm) + 1)
    decide :: forall p. (KnownNat p, 1 <= p, p <= n) => Proxy p -> ASheet n -> ASheet n
    decide _ = animate @p

mkClips :: forall a i. (Integral i, Enum a, Ord a, Bounded a) => (i, i) -> (i, i) -> Texture -> Map.Map a (SDL.Rectangle CInt)
mkClips (w, h) (x, y) (Texture _ ti) = Map.fromList $ zip es (rect <$> xs)
  where
    w' = fromIntegral w
    h' = fromIntegral h
    tw = SDL.textureWidth ti
    xs = iterate (fromIntegral . (+ w')) (fromIntegral x)
    es = [minBound .. maxBound]
    rect offset = mkRect x' y' w' h'
      where
        x' = offset `mod` tw
        y' = fromIntegral y + (offset `div` tw) * h'

withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize [SDL.InitVideo, SDL.InitAudio]
  void op
  SDL.quit

withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit

withSDLFont :: (MonadIO m) => m a -> m ()
withSDLFont op = do
  SDL.Font.initialize
  op
  SDL.Font.quit

withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w
  where
    p = SDL.defaultWindow {SDL.windowInitialSize = z}
    z = SDL.V2 (fromIntegral x) (fromIntegral y)

withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig
  void $ op r
  SDL.destroyRenderer r

rendererConfig :: SDL.RendererConfig
rendererConfig =
  SDL.RendererConfig
    { SDL.rendererType = SDL.AcceleratedVSyncRenderer,
      SDL.rendererTargetTexture = False
    }

isContinue :: Maybe SDL.Event -> Bool
isContinue = maybe True (not . isQuitEvent)

conditionallyRun :: (Monad m) => m a -> Bool -> m Bool
conditionallyRun f True = True <$ f
conditionallyRun _ False = pure False

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent _ = False

setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest

loadTexture :: (MonadIO m) => SDL.Renderer -> FilePath -> m Texture
loadTexture r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure $ mkTexture t i

destroyTexture :: (MonadIO m) => Texture -> m ()
destroyTexture (Texture t _) = SDL.destroyTexture t

mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)

mkRect :: a -> a -> a -> a -> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h
