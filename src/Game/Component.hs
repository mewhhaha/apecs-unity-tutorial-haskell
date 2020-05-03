{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Game.Component where

import Apecs
import Apecs.Experimental.Reactive
import Apecs.SDL.Internal (Texture)
import Data.Array
import Data.List.NonEmpty (NonEmpty (..))
import GHC.TypeNats
import Linear
import Linear.V2
import System.Random

defaultEnumRandomR :: (Enum a, RandomGen g) => (a, a) -> g -> (a, g)
defaultEnumRandomR (lo, hi) g = (toEnum i, g')
  where
    (i, g') = randomR (fromEnum lo, fromEnum hi) g

defaultBoundedRandom :: (Random a, Bounded a, RandomGen g) => g -> (a, g)
defaultBoundedRandom = randomR (minBound, maxBound)

data Player = PAttack | PHurt | PIdle
  deriving (Enum, Eq, Ord, Bounded)

data Direction = North | East | South | West
  deriving (Eq, Enum, Ord, Bounded)

instance Random Direction where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Zombie = ZIdle | ZAttack
  deriving (Enum, Bounded)

data Vampire = VIdle | VAttack
  deriving (Enum, Bounded)

data Happened = PlayerAttack | PlayerMove | PlayerHurt | PlayerDie | PlayerWin | EnemyHurt | EnemyAttack | EnemyDie | SodaPicked | FruitPicked | ObstacleHurt | ObstacleDie
  deriving (Eq, Ord)

data Prop = Soda | Fruit | Exit
  deriving (Eq, Ord, Enum, Bounded)

instance Random Prop where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Ground = G1 | G2 | G3 | G4 | G5 | G6 | G7 | G8
  deriving (Eq, Ord, Enum, Bounded)

instance Random Ground where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Wall = W1 | W2 | W3
  deriving (Eq, Ord, Enum, Bounded)

instance Random Wall where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Obstacle = O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8
  deriving (Eq, Ord, Enum, Bounded)

instance Random Obstacle where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

type Position = V2 Int

data Linear = Linear Double Position Position

newtype Stat
  = Stat
      { life :: Int
      }

newtype CTime = CTime Integer

instance Component CTime where type Storage CTime = Apecs.Global CTime

instance Semigroup CTime where (CTime t1) <> (CTime t2) = CTime (t1 + t2)

instance Monoid CTime where mempty = CTime 0

newtype CPosition = CPosition Position
  deriving (Show, Eq, Ord)
  deriving (Ix) via Position

instance Bounded CPosition where
  minBound = CPosition (V2 0 0)
  maxBound = CPosition (V2 20 15)

instance Component CPosition where type Storage CPosition = Reactive (IxMap CPosition) (Apecs.Map CPosition)

newtype CPlayer = CPlayer [Player]

instance Component CPlayer where type Storage CPlayer = Unique CPlayer

data CGoal = CGoal

instance Component CGoal where type Storage CGoal = Unique CGoal

newtype CStat = CStat Stat

instance Component CStat where type Storage CStat = Apecs.Map CStat

data CEnemy = CEnemy

instance Component CEnemy where type Storage CEnemy = Apecs.Map CEnemy

newtype CZombie = CZombie Zombie

instance Component CZombie where type Storage CZombie = Apecs.Map CZombie

newtype CVampire = CVampire Vampire

instance Component CVampire where type Storage CVampire = Apecs.Map CVampire

newtype Clip a = Clip a

type CGround = Clip Ground

instance Component CGround where type Storage CGround = Apecs.Map CGround

type CWall = Clip Wall

instance Component CWall where type Storage CWall = Apecs.Map CWall

data ObstacleHealth = ONew | ODamaged
  deriving (Eq, Ord)

newtype CObstacle = CObstacle Obstacle

instance Component CObstacle where type Storage CObstacle = Apecs.Map CObstacle

newtype CLinear = CLinear Linear

instance Component CLinear where type Storage CLinear = Apecs.Map CLinear

type CProp = Clip Prop

instance Component CProp where type Storage CProp = Apecs.Map CProp

data CFruit = CFruit

instance Component CFruit where type Storage CFruit = Apecs.Map CFruit

data CDead = CDead

instance Component CDead where type Storage CDead = Apecs.Map CDead

data CSoda = CSoda

instance Component CSoda where type Storage CSoda = Apecs.Map CSoda

newtype CLevel = CLevel Word

instance Component CLevel where type Storage CLevel = Apecs.Global CLevel

instance Semigroup CLevel where (CLevel t1) <> (CLevel t2) = CLevel (t1 + t2)

instance Monoid CLevel where mempty = CLevel 0

data CIsRunning = Running | Paused | Stopped
  deriving (Eq)

instance Component CIsRunning where type Storage CIsRunning = Apecs.Global CIsRunning

instance Semigroup CIsRunning where
  _ <> next = next

instance Monoid CIsRunning where mempty = Stopped

data CAnimation = CAnimation Double Double

instance Component CAnimation where type Storage CAnimation = Apecs.Map CAnimation

newtype CLatest = CLatest [Happened]

instance Component CLatest where type Storage CLatest = Apecs.Global CLatest

instance Semigroup CLatest where (CLatest t1) <> (CLatest t2) = CLatest (t1 <> t2)

instance Monoid CLatest where mempty = CLatest []

newtype GameOverlay = GameOverlay Texture

newtype LevelChangeOverlay = LevelChangeOverlay Texture

data Overlay = Game GameOverlay | LevelChange

newtype COverlay = COverlay [Overlay]

instance Component COverlay where type Storage COverlay = Apecs.Global COverlay

instance Semigroup COverlay where (COverlay t1) <> (COverlay t2) = COverlay (t1 <> t2)

instance Monoid COverlay where mempty = COverlay []
