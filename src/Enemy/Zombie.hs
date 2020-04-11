module Enemy.Zombie (idle, new, attack) where

import Apecs
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import Game.Component
import Game.World
import Linear (V2)

idle = CAnimation 0 0.8

attack = CAnimation 0 0.4

new :: V2 Double -> System' ()
new position = void $ newEntity (CEnemy, CStat Stat {hitpoints = 1}, CActionStream ([] :| []), CZombie ZIdle, idle, CPosition position)
