{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Env where

import Apecs.SDL.Internal (Animation, Sheet, mkRect, mkSheet)
import qualified Data.Map as Map
import Foreign.C.Types
import Game.Component
import qualified SDL

frames :: forall a i. (Integral i, Enum a, Ord a, Bounded a) => (i, i) -> Map.Map a (SDL.Rectangle CInt)
frames (w, h) = Map.fromList $ zipWith (\e x -> (e, mkRect (fromIntegral x) 0 (fromIntegral w) (fromIntegral h))) es xs
  where
    xs = iterate (+ w) 0
    es = [minBound .. maxBound]

data Zombie
  = Zombie
      { idle :: Animation 6,
        attack :: Animation 2
      }

data Vampire
  = Vampire
      { idle :: Animation 6,
        attack :: Animation 2
      }

data Player
  = Player
      { idle :: Animation 6,
        attack :: Animation 2,
        hurt :: Animation 2
      }

data Env
  = Env
      { zombie :: Zombie,
        vampire :: Vampire,
        player :: Player,
        prop :: Sheet Prop,
        ground :: Sheet Ground,
        obstacle :: Sheet Obstacle,
        wall :: Sheet Wall
      }
