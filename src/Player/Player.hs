module Player.Player (new, idle, attack, hurt, animate) where

import Apecs
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import Game.Component
import Game.World
import Linear (V2)

idle = CAnimation 0 0.5

attack = CAnimation 0 0.2

hurt = CAnimation 0 0.5

animate PHurt = hurt
animate PAttack = attack
animate PIdle = idle

new :: V2 Double -> System' ()
new position = void $ newEntity (CPlayer (PIdle :| []), CStat Stat {hitpoints = 10}, CActionStream ((0, []) :| []), CPosition position, idle)
