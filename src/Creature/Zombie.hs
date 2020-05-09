module Creature.Zombie (new, animation) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World

idle = CAnimation 0 0.8

attack = CAnimation 0 0.4

animation ZIdle = idle
animation ZAttack = attack

new :: Position -> System' ()
new position = void $ newEntity (CEnemy, CZombie ZIdle, idle, CMove 0 position position, CPosition position)
