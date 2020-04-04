module Main where

import Run
import System.World (initWorld)

main :: IO ()
main = do
  w <- initWorld
  run w
