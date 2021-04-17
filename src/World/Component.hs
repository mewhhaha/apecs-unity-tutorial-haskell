{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module World.Component where

import Apecs qualified hiding (Set)
import Apecs.Experimental.Reactive (IxMap, Reactive)
import Data.Array (Ix)
import Optics.TH (makeLenses)
import Relude
import Relude.Container qualified as Container
import SDL qualified
import SDL.Font qualified as SDLFont
import SDL.Mixer qualified as SDLMixer

-- Game components

type Position = SDL.V2 Int

newtype CPosition = CPosition Position
  deriving (Show, Eq, Ord)
  deriving (Ix) via Position

instance Bounded CPosition where
  minBound = CPosition (SDL.V2 0 0)
  maxBound = CPosition (SDL.V2 100 100)

instance Apecs.Component CPosition where type Storage CPosition = Reactive (IxMap CPosition) (Apecs.Map CPosition)

data CPlayer = CPlayer

instance Apecs.Component CPlayer where type Storage CPlayer = Apecs.Unique CPlayer

data Enemy = Zombie | Vampire | Obstacle

newtype CEnemy = CEnemy Enemy

instance Apecs.Component CEnemy where type Storage CEnemy = Apecs.Map CEnemy

data Food = Fruit | Soda

newtype CFood = CFood Food

instance Apecs.Component CFood where type Storage CFood = Apecs.Map CFood

newtype CLife = CLife Int

instance Apecs.Component CLife where type Storage CLife = Apecs.Map CLife

data CDead = CDead

instance Apecs.Component CDead where type Storage CDead = Apecs.Map CDead

-- Animation components

data AnimationName
  = PlayerIdle
  | PlayerHurt
  | PlayerAttack
  | ZombieIdle
  | ZombieAttack
  | VampireIdle
  | VampireAttack

data Animation = Animation
  { _timer :: Double,
    _name :: AnimationName,
    _speed :: Double
  }

makeLenses ''Animation

newtype CAnimation = CAnimation Animation

instance Apecs.Component CAnimation where type Storage CAnimation = Apecs.Map CAnimation

-- Resource components

data CResources = CResources
  { _fonts :: Container.Map (FilePath, Int) SDLFont.Font,
    _sprites :: Container.Map FilePath SDL.Texture,
    _sounds :: Container.Map FilePath SDLMixer.Chunk
  }

makeLenses ''CResources

instance Apecs.Component CResources where type Storage CResources = Apecs.Global CResources

instance Semigroup CResources where _ <> c2 = c2

instance Monoid CResources where
  mempty =
    CResources
      { _fonts = mempty,
        _sprites = mempty,
        _sounds = mempty
      }

-- Keyboard event components

data KeyboardState = KeyPressed | KeyDown | KeyReleased | KeyUp

data KeyboardInput = KeyboardInput SDL.Scancode KeyboardState

data GameInput = GameInput
  { moveLeft :: KeyboardInput,
    moveRight :: KeyboardInput,
    moveUp :: KeyboardInput,
    moveDown :: KeyboardInput,
    resetLevel :: KeyboardInput,
    toMenu :: KeyboardInput
  }

data MainMenuInput = MainMenuInput
  { pressUp :: KeyboardInput,
    pressDown :: KeyboardInput,
    selectOption :: KeyboardInput,
    toGame :: KeyboardInput
  }

newtype LevelTitleInput = LevelTitleInput
  {continue :: KeyboardInput}

data GameOverInput = GameOverInput
  {restart :: KeyboardInput, exit :: KeyboardInput}

defaultMainMenuInput :: MainMenuInput
defaultMainMenuInput =
  MainMenuInput
    { pressUp = KeyboardInput SDL.ScancodeUp KeyUp,
      pressDown = KeyboardInput SDL.ScancodeDown KeyUp,
      selectOption = KeyboardInput SDL.ScancodeReturn KeyUp,
      toGame = KeyboardInput SDL.ScancodeEscape KeyUp
    }

defaultGameInput :: GameInput
defaultGameInput =
  GameInput
    { moveLeft = KeyboardInput SDL.ScancodeLeft KeyUp,
      moveRight = KeyboardInput SDL.ScancodeRight KeyUp,
      moveUp = KeyboardInput SDL.ScancodeUp KeyUp,
      moveDown = KeyboardInput SDL.ScancodeDown KeyUp,
      resetLevel = KeyboardInput SDL.ScancodeR KeyUp,
      toMenu = KeyboardInput SDL.ScancodeEscape KeyUp
    }

defaultLevelTitleInput :: LevelTitleInput
defaultLevelTitleInput =
  LevelTitleInput
    { continue = KeyboardInput SDL.ScancodeReturn KeyUp
    }

defaultGameOverInput :: GameOverInput
defaultGameOverInput =
  GameOverInput
    { restart = KeyboardInput SDL.ScancodeReturn KeyUp,
      exit = KeyboardInput SDL.ScancodeEscape KeyUp
    }

-- Scene components

data MenuChoice = ExitGame | StartGame
  deriving (Enum, Bounded)

data Scene = MainMenu MainMenuInput MenuChoice | LevelTitle LevelTitleInput | Game GameInput | GameOver GameOverInput

newtype CScene = CScene Scene

instance Apecs.Component CScene where type Storage CScene = Apecs.Global CScene

instance Semigroup CScene where _ <> c2 = c2

instance Monoid CScene where
  mempty = CScene $ MainMenu defaultMainMenuInput StartGame

-- Level components

newtype Level = Level Int

newtype CLevel = CLevel Level

instance Apecs.Component CLevel where type Storage CLevel = Apecs.Global CLevel

instance Semigroup CLevel where _ <> c2 = c2

instance Monoid CLevel where
  mempty = CLevel $ Level 0

-- Event components

data Happening
  = NextLevel
  | Move {source :: Apecs.Entity, from :: SDL.V2 Int, to :: SDL.V2 Int}
  | Attack {source :: Apecs.Entity, damage :: Int, target :: Apecs.Entity}
  | Pickup {source :: Apecs.Entity, life :: Int, target :: Apecs.Entity}
  deriving (Ord, Eq)

type Happenings = [Happening]

newtype CHappenings = CHappenings Happenings

instance Apecs.Component CHappenings where type Storage CHappenings = Apecs.Global CHappenings

instance Semigroup CHappenings where (CHappenings c1) <> (CHappenings c2) = CHappenings (c1 <> c2)

instance Monoid CHappenings where
  mempty = CHappenings mempty

-- Draw components

data LerpPosition = LerpPosition {old :: Position, current :: Position}

newtype CLerpPosition = CLerpPosition LerpPosition

instance Apecs.Component CLerpPosition where type Storage CLerpPosition = Apecs.Map CLerpPosition