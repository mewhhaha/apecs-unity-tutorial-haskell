{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Game.World where

import Apecs
import Control.Monad.Fail
import Game.Component

makeWorld
  "World"
  [ ''CPosition,
    ''CPlayer,
    ''CDrawable,
    ''CTime,
    ''CIsRunning,
    ''CAnimation,
    ''CGround,
    ''CWall,
    ''CObstacle,
    ''CProp,
    ''CGoal,
    ''CEnemy,
    ''CVampire,
    ''CZombie,
    ''CActionStream,
    ''CStat,
    ''CFruit,
    ''CSoda
  ]

type System' a = System World a

instance MonadFail (SystemT World IO) where
  fail s = lift (Prelude.fail s)
