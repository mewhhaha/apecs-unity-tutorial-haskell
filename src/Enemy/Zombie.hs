module Enemy.Zombie (idle, new) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World
import Linear (V2)

idle = CAnimation 0 0.8

new :: V2 Double -> System' ()
new position = void $ newEntity (CEnemy (Enemy {hitpoints = 1}), CZombie ZIdle, idle, CPosition position)
