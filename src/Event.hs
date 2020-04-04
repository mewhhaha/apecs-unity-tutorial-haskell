module Event where

import Apecs
import Control.Monad (when)
import Game.Component (CIsRunning (..))
import Game.World (System')
import qualified SDL

whenKeyPressed :: SDL.Scancode -> SDL.EventPayload -> System' () -> System' ()
whenKeyPressed s e = when (isKeyPressed s e)

isKeyPressed :: SDL.Scancode -> SDL.EventPayload -> Bool
isKeyPressed sc ke@(SDL.KeyboardEvent e) = justPressed && isKeyDown sc ke
  where
    justPressed = not (SDL.keyboardEventRepeat e)
isKeyPressed _ _ = False

isKeyDown :: SDL.Scancode -> SDL.EventPayload -> Bool
isKeyDown scancode (SDL.KeyboardEvent e) = pressed && rightKey
  where
    pressed = SDL.keyboardEventKeyMotion e == SDL.Pressed
    rightKey = scancode == SDL.keysymScancode (SDL.keyboardEventKeysym e)
isKeyDown _ _ = False

whenGameIsRunning :: System' () -> System' ()
whenGameIsRunning s = do
  isRunning <- (/= Stopped) <$> get global
  when isRunning s
