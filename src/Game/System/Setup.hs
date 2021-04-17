module Game.System.Setup (system) where

import Apecs (SystemT)
import Relude
import World (World)

system :: SystemT World IO ()
system = do
  pass