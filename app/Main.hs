module Main where

import Game.World (initWorld)
import Run

main :: IO ()
main = do
  w <- initWorld
  run w
