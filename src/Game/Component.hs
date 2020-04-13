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
import Data.List.NonEmpty (NonEmpty)
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

data Action = Hurt Word | Attack | Movement (V2 Double)
  deriving (Eq, Ord)

data Prop = Drink | Apples | Exit
  deriving (Eq, Ord, Enum, Bounded)

instance Random Prop where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Ground = G1 | G2 | G3 | G4 | G5 | G6 | G7 | G8
  deriving (Eq, Ord, Enum, Bounded)

instance Random Ground where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Obstacle = O1 | O2 | O3 | O4 | O5 | O6 | O7
  deriving (Eq, Ord, Enum, Bounded)

instance Random Obstacle where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Wall = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W10 | W11
  deriving (Eq, Ord, Enum, Bounded)

instance Random Wall where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

type Position = V2 Double

data CDrawable = Drawable

newtype Stat
  = Stat
      { hitpoints :: Int
      }

newtype CTime = CTime Integer

newtype CPosition = CPosition Position

newtype CPlayer = CPlayer (NonEmpty Player)

data CGoal = CGoal

newtype CStat = CStat Stat

data CEnemy = CEnemy

newtype CZombie = CZombie Zombie

newtype CVampire = CVampire Vampire

newtype Clip a = Clip a

type CGround = Clip Ground

type CWall = Clip Wall

type CObstacle = Clip Obstacle

type CProp = Clip Prop

data CIsRunning = Running | Paused | Stopped
  deriving (Eq)

data CAnimation = CAnimation Double Double

newtype CActionStream = CActionStream (NonEmpty (Integer, [Action]))

instance Component CPosition where type Storage CPosition = Apecs.Map CPosition

instance Component CStat where type Storage CStat = Apecs.Map CStat

instance Component CActionStream where type Storage CActionStream = Apecs.Map CActionStream

instance Component CDrawable where type Storage CDrawable = Apecs.Map CDrawable

instance Component CAnimation where type Storage CAnimation = Apecs.Map CAnimation

instance Component CGround where type Storage CGround = Apecs.Map CGround

instance Component CWall where type Storage CWall = Apecs.Map CWall

instance Component CObstacle where type Storage CObstacle = Apecs.Map CObstacle

instance Component CEnemy where type Storage CEnemy = Apecs.Map CEnemy

instance Component CVampire where type Storage CVampire = Apecs.Map CVampire

instance Component CZombie where type Storage CZombie = Apecs.Map CZombie

instance Component CProp where type Storage CProp = Apecs.Map CProp

instance Component CTime where type Storage CTime = Apecs.Global CTime

instance Semigroup CTime where (CTime t1) <> (CTime t2) = CTime (t1 + t2)

instance Monoid CTime where mempty = CTime 0

instance Component CIsRunning where type Storage CIsRunning = Apecs.Global CIsRunning

instance Semigroup CIsRunning where
  _ <> next = next

instance Monoid CIsRunning where mempty = Stopped

instance Component CPlayer where type Storage CPlayer = Unique CPlayer

instance Component CGoal where type Storage CGoal = Unique CGoal
