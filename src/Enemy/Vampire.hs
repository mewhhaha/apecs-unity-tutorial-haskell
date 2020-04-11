module Enemy.Vampire (idle, new, attack) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World
import Linear (V2)

idle = CAnimation 0 0.5

attack = CAnimation 0 0.4

new :: V2 Double -> System' ()
new position = void $ newEntity (CEnemy, CStat Stat {hitpoints = 2}, CActions [], CVampire VIdle, idle, CPosition position)
