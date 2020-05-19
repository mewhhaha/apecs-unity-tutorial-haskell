module Reload where

import Rapid
import Run

-- Read https://hackage.haskell.org/package/rapid-0.1.4/docs/Rapid.html for more information about hot reloading
update :: IO ()
update =
  rapid 0 $ \r -> restart r "game" (run $ Just r)
