module Creature.Player (new, animation) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World

idle = CAnimation 0 0.5

attack = CAnimation 0 0.2

hurt = CAnimation 0 0.5

animation PHurt = hurt
animation PAttack = attack
animation PIdle = idle

new :: Position -> System' ()
new position = void $ newEntity (CPlayer [PIdle], CStat Stat {life = 100}, CPosition position, CMove 0 position position, idle)
