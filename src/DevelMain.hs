module DevelMain where

import Rapid
import Run

update :: IO ()
update =
  rapid 0 $ \r -> restart r "game" (run $ Just r)
