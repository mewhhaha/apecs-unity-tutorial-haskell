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
import Game.Component

makeWorld
  "World"
  [ ''CPosition,
    ''CPlayer,
    ''CDrawable,
    ''CTime,
    ''CIsRunning,
    ''CAnimation,
    ''CGround
  ]

type System' a = System World a
