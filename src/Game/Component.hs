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
import Data.Array
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

data Prop = Drink | Apples | Exit
  deriving (Eq, Ord, Enum, Bounded)

data Ground = G1 | G2 | G3 | G4 | G5 | G6 | G7 | G8
  deriving (Eq, Ord, Enum, Bounded)

instance Random Ground where
  randomR = defaultEnumRandomR
  random = defaultBoundedRandom

data Obstacle = O1 | O2 | O3 | O4 | O5 | O6 | O7
  deriving (Eq, Ord, Enum, Bounded)

data Wall = W1 | W2 | W3 | W4 | W5 | W6 | W7 | W8 | W9 | W10 | W11
  deriving (Eq, Ord, Enum, Bounded)

type Position = V2 Double

data CDrawable = Drawable

newtype CTime = CTime Integer

newtype CPosition = CPosition Position
  deriving (Eq, Ord)

data CPlayer = CPlayer

newtype CGround = CGround Ground

data CIsRunning = Running | Paused | Stopped
  deriving (Eq)

data CAnimation = CAnimation Double Double

instance Component CPosition where type Storage CPosition = Apecs.Map CPosition

instance Component CDrawable where type Storage CDrawable = Apecs.Map CDrawable

instance Component CAnimation where type Storage CAnimation = Apecs.Map CAnimation

instance Component CGround where type Storage CGround = Apecs.Map CGround

instance Component CTime where type Storage CTime = Apecs.Global CTime

instance Semigroup CTime where (CTime t1) <> (CTime t2) = CTime (t1 + t2)

instance Monoid CTime where mempty = CTime 0

instance Component CIsRunning where type Storage CIsRunning = Apecs.Global CIsRunning

instance Semigroup CIsRunning where
  _ <> next = next

instance Monoid CIsRunning where mempty = Stopped

instance Component CPlayer where type Storage CPlayer = Unique CPlayer
