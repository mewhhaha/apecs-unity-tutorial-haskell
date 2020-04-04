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

import Apecs (ask, cfoldM, cmap, lift, newEntity, runWith)
import Apecs.Extra (cM)
import Apecs.SDL (play, renderSprite)
import Apecs.SDL.Internal (Animation, Sheet (..), Sprite, Texture, animate, linear, loadTexture, mkAnimation, mkRect, mkSheet)
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
  newEntity (CPlayer, CPosition (V2 0 0), CAnimation 0 0.5)
  newEntity (CGround G1, CPosition (V2 3 3))
  sequence_ $
    newEntity . (CGround *** CPosition)
      <$> zip
        (randoms (mkStdGen 0))
        [V2 x y | x <- [0 .. 19], y <- [0 .. 19]]

resources :: SDL.Renderer -> IO Env.Env
resources r = do
  prop <- load32x32 "props.png"
  ground <- load32x32 "ground.png"
  obstacle <- load32x32 "obstacles.png"
  wall <- load32x32 "obstacles.png"
  playerAttack <- loadAnimation "player_attack.png"
  playerIdle <- loadAnimation "player_idle.png"
  playerHurt <- loadAnimation "player_hurt.png"
  vampireIdle <- loadAnimation "vampire_idle.png"
  vampireAttack <- loadAnimation "vampire_attack.png"
  zombieIdle <- loadAnimation "zombie_idle.png"
  zombieAttack <- loadAnimation "zombie_attack.png"
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
    load32x32 :: forall a. (Enum a, Ord a, Bounded a) => FilePath -> IO (Sheet a)
    load32x32 f = (`mkSheet` Env.frames @a (32, 32)) <$> load f
    loadAnimation :: forall n. (KnownNat n, 1 <= n) => FilePath -> IO (Animation n)
    loadAnimation f = mkAnimation @n <$> load f

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
draw Env.Env {player, ground} r = do
  draws <- sequence [drawGround ground, drawPlayer player]
  lift . sequence_ $ draws
  where
    toScreen :: Integral a => V2 Double -> V2 a
    toScreen p = round . (* 32) <$> p
    render :: forall a i. (Sprite a) => V2 Double -> a -> IO ()
    render p = renderSprite r (toScreen p)
    drawGround :: Sheet Ground -> System' (IO ())
    drawGround s = cM $ \(CGround g, CPosition pos) ->
      render pos (s {clip = Just g})
    drawPlayer :: Env.Player -> System' (IO ())
    drawPlayer Env.Player {idle} =
      cM $ \(CPlayer, CPosition pos, CAnimation time dur) ->
        render pos (linear time dur idle)

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
