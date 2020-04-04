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

module System.World where

import Apecs
import System.Component (CAnimation, CDrawable, CIsRunning, CPlayer, CPosition, CTime)

makeWorld
  "World"
  [ ''CPosition,
    ''CPlayer,
    ''CDrawable,
    ''CTime,
    ''CIsRunning,
    ''CAnimation
  ]

type System' a = System World a
