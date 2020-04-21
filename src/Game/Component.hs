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

data Happened = PlayerAttack | PlayerMove | PlayerHurt | PlayerDie | PlayerWin | EnemyHurt | EnemyAttack | EnemyDie | SodaPicked | FruitPicked
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

data Wall = W1 | W2 | W3 | W4 | W5 | W6 | W7
  deriving (Eq, Ord, Enum, Bounded)

instance Random Wall where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Obstacle = O1 | O2 | O3 | O4 | O5 | O6 | O7 | O8 | O9 | O10 | O11
  deriving (Eq, Ord, Enum, Bounded)

instance Random Obstacle where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

type Position = V2 Int

newtype Stat
  = Stat
      { hitpoints :: Int
      }

newtype CTime = CTime Integer

newtype CPosition = CPosition Position
  deriving (Show, Eq, Ord)
  deriving (Ix) via Position

instance Bounded CPosition where
  minBound = CPosition (V2 0 0)
  maxBound = CPosition (V2 20 15)

newtype CPlayer = CPlayer [Player]

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

data CFruit = CFruit

data CDead = CDead

data CSoda = CSoda

newtype CLevel = CLevel Word

data CIsRunning = Running | Paused | Stopped
  deriving (Eq)

data CAnimation = CAnimation Double Double

newtype CLatest = CLatest [Happened]

instance Component CPosition where type Storage CPosition = Reactive (IxMap CPosition) (Apecs.Map CPosition)

instance Component CDead where type Storage CDead = Apecs.Map CDead

instance Component CSoda where type Storage CSoda = Apecs.Map CSoda

instance Component CFruit where type Storage CFruit = Apecs.Map CFruit

instance Component CStat where type Storage CStat = Apecs.Map CStat

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

instance Component CLatest where type Storage CLatest = Apecs.Global CLatest

instance Semigroup CLatest where (CLatest t1) <> (CLatest t2) = CLatest (t1 <> t2)

instance Monoid CLatest where mempty = CLatest []

instance Component CLevel where type Storage CLevel = Apecs.Global CLevel

instance Semigroup CLevel where (CLevel t1) <> (CLevel t2) = CLevel (t1 + t2)

instance Monoid CLevel where mempty = CLevel 0

instance Component CIsRunning where type Storage CIsRunning = Apecs.Global CIsRunning

instance Semigroup CIsRunning where
  _ <> next = next

instance Monoid CIsRunning where mempty = Stopped

instance Component CPlayer where type Storage CPlayer = Unique CPlayer

instance Component CGoal where type Storage CGoal = Unique CGoal
