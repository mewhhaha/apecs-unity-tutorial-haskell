module Game where

import Apecs qualified
import Game.System.Draw qualified as Draw
import Game.System.Input qualified as Input
import Game.System.Logic qualified as Logic
import Game.System.Overlay qualified as Overlay
import Game.System.Setup qualified as Setup
import Relude
import SDL qualified
import Types (Step, Tick (..))
import World (World, initWorld)

initGame :: IO World
initGame = do
  w <- initWorld
  Apecs.runWith w (Setup.system >> ask)

step :: Step IO World
step = do
  renderer <- asks readRenderer
  world <- asks readWorld
  SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 0
  Apecs.runWith world (Input.system >> Logic.system >> Draw.system >> Overlay.system >> ask)
