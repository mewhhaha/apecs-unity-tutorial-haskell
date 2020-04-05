{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Run where

import Apecs (Get, Members, ask, cfoldM, cmap, lift, newEntity, runWith)
import Apecs.SDL (play, renderSprite)
import Apecs.SDL.Internal (ASheet, Sheet (..), Sprite, Texture, animate, linear, loadTexture, mkASheet, mkClips, mkRect, mkSheet)
import Control.Arrow
import Control.Monad (forM_)
import Data.Either
import qualified Data.Map as Map
import qualified Env
import Event (isKeyDown)
import GHC.TypeNats
import Game.Component
import Game.World (System', World)
import Linear (V2 (..))
import qualified SDL
import System.FilePath.Posix ((</>))
import System.Random

data Direction = North | East | South | West

data Action = Move Direction | Quit

initialize :: System' ()
initialize = do
  let shrink = tail . init
      hedges = [V2 x y | x <- xs, y <- [head ys, last ys]]
      vedges = [V2 x y | x <- [head xs, last xs], y <- shrink ys]
  sequence_ $
    newEntity . (Clip *** CPosition)
      <$> zip (enums @Ground 0) [V2 x y | x <- shrink xs, y <- shrink ys]
  sequence_ $
    newEntity . (Clip *** CPosition)
      <$> zip (enums @Wall 1) (hedges ++ vedges)
  let startX = pick (shrink xs) 1
      startY = pick (shrink ys) 2
  newEntity (CPlayer, CPosition (V2 startX startY), CAnimation 0 0.5)
  pure ()
  where
    ends :: [Double] -> (Int, Int)
    ends l = (floor $ head l, floor $ last l)
    pick :: [Double] -> Int -> Double
    pick rs seed = fromIntegral . fst . randomR (ends rs) $ mkStdGen seed
    enums :: forall a. Random a => Int -> [a]
    enums = randoms . mkStdGen
    xs :: [Double]
    xs = [0 .. 19]
    ys :: [Double]
    ys = [0 .. 14]

resources :: SDL.Renderer -> IO Env.Env
resources r = do
  prop <- loadSheet32x32 "props.png"
  ground <- loadSheet32x32 "ground.png"
  obstacle <- loadSheet32x32 "obstacles.png"
  wall <- loadSheet32x32 "wall.png"
  playerAttack <- loadASheet32x32 "player_attack.png"
  playerIdle <- loadASheet32x32 "player_idle.png"
  playerHurt <- loadASheet32x32 "player_hurt.png"
  vampireIdle <- loadASheet32x32 "vampire_idle.png"
  vampireAttack <- loadASheet32x32 "vampire_attack.png"
  zombieIdle <- loadASheet32x32 "zombie_idle.png"
  zombieAttack <- loadASheet32x32 "zombie_attack.png"
  pure
    Env.Env
      { prop = prop,
        ground = ground,
        obstacle = obstacle,
        wall = wall,
        player =
          Env.Player
            { attack = playerAttack,
              idle = playerIdle,
              hurt = playerHurt
            },
        vampire =
          Env.Vampire
            { attack = vampireAttack,
              idle = vampireIdle
            },
        zombie =
          Env.Zombie
            { attack = zombieAttack,
              idle = zombieIdle
            }
      }
  where
    load :: FilePath -> IO Texture
    load f = loadTexture r ("resources" </> "sprites" </> f)
    loadSheet32x32 :: forall a. (Enum a, Ord a, Bounded a) => FilePath -> IO (Sheet a)
    loadSheet32x32 f = (`mkSheet` mkClips (32, 32)) <$> load f
    loadASheet32x32 :: forall n. (KnownNat n, 1 <= n) => FilePath -> IO (ASheet n)
    loadASheet32x32 f = (`mkASheet` (32, 32)) <$> load f

movement :: Either Action SDL.Event -> Either Action SDL.Event
movement e = do
  payload <- SDL.eventPayload <$> e
  let pressed c r = if isKeyDown c payload then Left r else pure ()
  pressed SDL.ScancodeRight (Move East)
  pressed SDL.ScancodeLeft (Move West)
  pressed SDL.ScancodeUp (Move North)
  pressed SDL.ScancodeDown (Move South)
  e

events :: Env.Env -> [SDL.Event] -> [Action]
events _ es = lefts $ movement . Right <$> es

draw :: Env.Env -> SDL.Renderer -> System' ()
draw Env.Env {player, ground, wall} r = do
  draws <- sequence [drawSheet ground, drawSheet wall, drawPlayer player]
  lift . sequence_ $ draws
  where
    toScreen :: Integral a => V2 Double -> V2 a
    toScreen p = round . (* 32) <$> p
    render :: forall s. (Sprite s) => (V2 Double, s) -> IO ()
    render (p, a) = renderSprite r (toScreen p) a
    cdraw :: forall c s. (Sprite s, Members World IO c, Get World IO c) => (c -> (V2 Double, s)) -> System' (IO ())
    cdraw f = cfoldM (\acc c -> pure (acc >> render (f c))) mempty
    drawSheet :: forall c. (Members World IO (Clip c), Get World IO (Clip c), Ord c) => Sheet c -> System' (IO ())
    drawSheet s = cdraw $ \(Clip c, CPosition pos) ->
      (pos, (s {clip = Just c}))
    drawPlayer :: Env.Player -> System' (IO ())
    drawPlayer Env.Player {idle} =
      cdraw $ \(CPlayer, CPosition pos, CAnimation time dur) ->
        (pos, linear time dur idle)

step :: Env.Env -> Double -> [Action] -> System' World
step _ dt es = do
  cmap $ \(CPlayer, CPosition prev) -> Just (CPlayer, CPosition $ foldr update prev es)
  cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)
  ask
  where
    distance = 1
    update (Move East) p = p + V2 distance 0
    update (Move West) p = p + V2 (- distance) 0
    update (Move South) p = p + V2 0 distance
    update (Move North) p = p + V2 0 (- distance)
    update _ p = p

run :: World -> IO ()
run w = do
  w' <- runWith w (initialize >> ask)
  play w' resources events step draw
