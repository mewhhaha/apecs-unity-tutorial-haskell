{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Env where

import Apecs.SDL.Internal (ASheet, Sheet, mkRect, mkSheet)
import qualified Data.Map as Map
import Foreign.C.Types
import Game.Component
import qualified SDL

data Zombie
  = Zombie
      { idle :: ASheet 6,
        attack :: ASheet 2
      }

data Vampire
  = Vampire
      { idle :: ASheet 6,
        attack :: ASheet 2
      }

data Player
  = Player
      { idle :: ASheet 6,
        attack :: ASheet 2,
        hurt :: ASheet 2
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
