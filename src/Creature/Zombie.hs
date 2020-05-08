module Creature.Zombie (new, animate) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World

idle = CAnimation 0 0.8

attack = CAnimation 0 0.4

animate ZIdle = idle
animate ZAttack = attack

new :: Position -> System' ()
new position = void $ newEntity (CEnemy, CZombie ZIdle, idle, CInterpolate (Interpolate 0 position position), CPosition position)
