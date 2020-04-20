module Creature.Zombie (idle, new, attack, animate) where

import Apecs
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import Game.Component
import Game.World
import Linear (V2)

idle = CAnimation 0 0.8

attack = CAnimation 0 0.4

animate ZIdle = idle
animate ZAttack = attack

new :: V2 Double -> System' ()
new position = void $ newEntity (CEnemy, CStat Stat {hitpoints = 1}, CZombie ZIdle, idle, CPosition position)
