module Event (parseEvents, Event (..)) where

import Control.Monad (when)
import Data.Either (lefts)
import Game.Component (Direction (..))
import qualified SDL

data Event = InputMove Direction | InputQuit | EnterPressed
  deriving (Eq)

isKeyDown :: SDL.Scancode -> SDL.EventPayload -> Bool
isKeyDown scancode (SDL.KeyboardEvent e) = pressed && rightKey
  where
    pressed = SDL.keyboardEventKeyMotion e == SDL.Pressed
    rightKey = scancode == SDL.keysymScancode (SDL.keyboardEventKeysym e)
isKeyDown _ _ = False

movement :: Either Event SDL.Event -> Either Event SDL.Event
movement e = do
  payload <- SDL.eventPayload <$> e
  let down c r = when (isKeyDown c payload) (Left r)
  down SDL.ScancodeRight (InputMove East)
  down SDL.ScancodeLeft (InputMove West)
  down SDL.ScancodeUp (InputMove North)
  down SDL.ScancodeDown (InputMove South)
  e

enter :: Either Event SDL.Event -> Either Event SDL.Event
enter e = do
  payload <- SDL.eventPayload <$> e
  let down c r = when (isKeyDown c payload) (Left r)
  mapM_ (`down` EnterPressed) [SDL.ScancodeReturn, SDL.ScancodeReturn2, SDL.ScancodeKPEnter]
  e

parseEvents :: [SDL.Event] -> [Event]
parseEvents es = lefts (enter . movement . Right <$> es)
