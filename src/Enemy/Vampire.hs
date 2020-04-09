module Enemy.Vampire (idle, new) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World
import Linear (V2)

idle = CAnimation 0 0.5

new :: V2 Double -> System' ()
new position = void $ newEntity (CEnemy (Enemy {hitpoints = 2}), CVampire VIdle, idle, CPosition position)
