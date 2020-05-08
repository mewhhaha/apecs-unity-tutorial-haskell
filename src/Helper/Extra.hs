{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Helper.Extra where

import Apecs
import Apecs.Experimental.Reactive (ixLookup, withReactive)
import Control.Monad (filterM, when)
import Data.Maybe (listToMaybe)
import Game.Component
import Game.World (System', World)

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition op = (`when` op) =<< condition

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

toTime :: Double -> Integer
toTime dt = floor (dt * 10000)

eitherIf :: (a -> Bool) -> a -> Either () a
eitherIf f a = if f a then Right a else Left ()

maybeIf :: (a -> Bool) -> a -> Maybe a
maybeIf f a = if f a then Just a else Nothing

hasAny :: forall c. Get World IO c => [Entity] -> System' Bool
hasAny = fmap (not . null) . filterM (`exists` Proxy @c)

getAny :: forall c. Get World IO c => [Entity] -> System' (Maybe Entity)
getAny = fmap listToMaybe . filterM (`exists` Proxy @c)

entitiesAt :: Position -> System' [Entity]
entitiesAt pos = withReactive $ ixLookup (CPosition pos)
