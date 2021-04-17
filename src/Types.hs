module Types where

import Relude
import SDL qualified

data Tick a = Tick {readDeltaTime :: Double, readEvents :: [SDL.Event], readWorld :: a, readWindow :: SDL.Window, readRenderer :: SDL.Renderer}

data Command = End | Reset

type Step m a = ReaderT (Tick a) (ExceptT Command m) a