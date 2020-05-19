module Reload where

import Control.Concurrent
import Rapid
import Run

-- Read https://hackage.haskell.org/package/rapid-0.1.4/docs/Rapid.html for more information about reloading
update :: IO ()
update = do
  threadDelay 1000000
  rapid 0 $ \r -> restart r "game" (run $ Just r)
