{-# LANGUAGE FlexibleContexts #-}

module Apecs.Extra where

import Apecs

cM f = cfoldM (\acc c -> pure (acc >> f c)) mempty
