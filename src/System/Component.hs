{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

module System.Component
  ( Position,
    CDrawable (..),
    CTime (..),
    CPosition (..),
    CPlayer (..),
    CIsRunning (..),
    CAnimation (..),
  )
where

import Apecs
import Data.Array
import Linear
import Linear.V2

type Position = V2 Double

data CDrawable = Drawable

newtype CTime = CTime Integer

newtype CPosition = CPosition Position
  deriving (Eq, Ord)

data CPlayer = CPlayer deriving (Show)

data CIsRunning = Running | Paused | Stopped
  deriving (Eq)

data CAnimation = CAnimation Double Double

instance Component CPosition where type Storage CPosition = Apecs.Map CPosition

instance Component CDrawable where type Storage CDrawable = Apecs.Map CDrawable

instance Component CAnimation where type Storage CAnimation = Apecs.Map CAnimation

instance Component CTime where type Storage CTime = Apecs.Global CTime

instance Semigroup CTime where (CTime t1) <> (CTime t2) = CTime (t1 + t2)

instance Monoid CTime where mempty = CTime 0

instance Component CIsRunning where type Storage CIsRunning = Apecs.Global CIsRunning

instance Semigroup CIsRunning where
  _ <> next = next

instance Monoid CIsRunning where mempty = Stopped

instance Component CPlayer where type Storage CPlayer = Unique CPlayer
