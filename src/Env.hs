{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Env where

import Apecs.SDL.Internal (HSheet, Sheet, mkRect, mkSheet)
import qualified Data.Map as Map

data Prop = Drink | Apples | Exit
  deriving (Eq, Ord)

propsFrames = Map.fromList [(Drink, mkRect (x 0) 0 w h), (Apples, mkRect (x 1) 0 w h), (Exit, mkRect (x 2) 0 w h)]
  where
    w = 32
    h = 32
    x i = w * i

data Ent = Z | V | P

data Zombie
  = Zombie
      { idle :: HSheet 6,
        attack :: HSheet 2
      }

data Vampire
  = Vampire
      { idle :: HSheet 6,
        attack :: HSheet 2
      }

data Player
  = Player
      { idle :: HSheet 6,
        attack :: HSheet 2,
        hurt :: HSheet 2
      }

newtype Ground = Ground {ground :: HSheet 8}

newtype Obstacle = Obstacle {obstacle :: HSheet 7}

newtype Wall = Wall {wall :: HSheet 11}

data Env
  = Env
      { zombie :: Zombie,
        vampire :: Vampire,
        player :: Player,
        prop :: Sheet Prop,
        ground :: Ground,
        obstacle :: Obstacle,
        wall :: Wall
      }

mkProp p = mkSheet p propsFrames
