module Player.Player (new, idle, attack, hurt) where

import Apecs
import Control.Monad (void)
import Game.Component
import Game.World
import Linear (V2)

idle = CAnimation 0 0.5

attack = CAnimation 0 0.2

hurt = CAnimation 0 0.2

new :: V2 Double -> System' ()
new position = void $ newEntity (CPlayer PIdle, CActions [], CPosition position, idle)
