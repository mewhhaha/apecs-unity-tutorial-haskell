module Creature.Vampire (new, animate) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World

idle = CAnimation 0 0.5

attack = CAnimation 0 0.4

animate VIdle = idle
animate VAttack = attack

new :: Position -> System' ()
new position = void $ newEntity (CEnemy, CVampire VIdle, idle, CInterpolate (Interpolate 0 position position), CPosition position)
