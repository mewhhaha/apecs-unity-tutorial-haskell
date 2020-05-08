module Creature.Player (new, animate) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World

idle = CAnimation 0 0.5

attack = CAnimation 0 0.2

hurt = CAnimation 0 0.5

animate PHurt = hurt
animate PAttack = attack
animate PIdle = idle

new :: Position -> System' ()
new position = void $ newEntity (CPlayer [PIdle], CStat Stat {life = 100}, CPosition position, CInterpolate (Interpolate 0 position position), idle)
