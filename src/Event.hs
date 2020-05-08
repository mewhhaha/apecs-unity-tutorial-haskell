module Event (events, Event (..)) where

import Apecs
import Control.Monad (when)
import Data.Either (lefts)
import Data.Maybe (mapMaybe)
import qualified Env
import Game.Component (Direction (..))
import Game.World (System')
import qualified SDL

data Event = InputMove Direction | InputQuit | EnterPressed
  deriving (Eq)

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
  down SDL.ScancodeReturn EnterPressed
  e

events :: Env.Env -> [SDL.Event] -> [Event]
events _ es = lefts (enter . movement . Right <$> es)
