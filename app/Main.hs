module Main where

import Game (initGame, step)
import Play (play)
import Relude
import qualified SDL

main :: IO ()
main =
  play
    step
    "At any cost"
    (SDL.V2 640 480)
    initGame
