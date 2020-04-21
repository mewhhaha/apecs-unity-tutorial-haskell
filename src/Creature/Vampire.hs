module Creature.Vampire (idle, new, attack, animate) where

import Apecs
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import Game.Component
import Game.World
import Linear (V2)

idle = CAnimation 0 0.5

attack = CAnimation 0 0.4

animate VIdle = idle
animate VAttack = attack

new :: Position -> System' ()
new position = void $ newEntity (CEnemy, CStat Stat {hitpoints = 2}, CVampire VIdle, idle, CPosition position)
