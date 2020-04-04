{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Env where

import Apecs.SDL.Internal (HSheet, Sheet, mkRect, mkSheet)
import qualified Data.Map as Map

data Props = Drink | Apples | Exit
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

data Env
  = Env
      { zombie :: Zombie,
        vampire :: Vampire,
        player :: Player,
        props :: Sheet Props,
        ground :: HSheet 8,
        obstacles :: HSheet 7,
        wall :: HSheet 11
      }

mkProps p = mkSheet p propsFrames
