{-# LANGUAGE TemplateHaskell #-}

module World where

import Apecs
import Relude
import Types (Command, Tick)
import World.Component

makeWorld "World" [''CResources, ''CLevel, ''CScene, ''CPosition, ''CPlayer, ''CEnemy, ''CFood, ''CHappenings, ''CAnimation, ''CLife, ''CDead, ''CLerpPosition]

type System' a = (SystemT World (ReaderT (Tick World) (ExceptT Command IO))) a

instance MonadFail (SystemT World (ReaderT (Tick World) (ExceptT Command IO))) where
  fail s = lift (fail s)

tickState :: (Tick World -> a) -> System' a
tickState f = lift $ asks f