module Creature.Vampire (new, animation) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World

idle :: CAnimation
idle = CAnimation 0 0.5

attack :: CAnimation
attack = CAnimation 0 0.4

animation :: Vampire -> CAnimation
animation VIdle = idle
animation VAttack = attack

new :: Position -> System' ()
new position = void $ newEntity (CEnemy, CVampire VIdle, idle, CMove 0 position position, CPosition position)
