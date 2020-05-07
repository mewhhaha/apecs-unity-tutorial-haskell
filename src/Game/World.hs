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
    ''CTime,
    ''CGame,
    ''CAnimation,
    ''CGround,
    ''CWall,
    ''CObstacle,
    ''CProp,
    ''CGoal,
    ''CEnemy,
    ''CVampire,
    ''CZombie,
    ''CStat,
    ''CFruit,
    ''CSoda,
    ''CDead,
    ''CLinear,
    ''CLevel,
    ''CLatest,
    ''CGameOverlay,
    ''CLevelOverlay,
    ''CDeathOverlay
  ]

type System' a = System World a

instance MonadFail (SystemT World IO) where
  fail s = lift (Prelude.fail s)

type All =
  ( ( CPosition,
      CPlayer,
      CDead,
      CAnimation,
      CSoda
    ),
    ( CGround,
      CWall,
      CObstacle,
      CProp,
      CGoal,
      CEnemy
    ),
    ( CVampire,
      CZombie,
      CLinear,
      CStat,
      CFruit
    )
  )
