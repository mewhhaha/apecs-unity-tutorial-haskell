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
import Apecs.SDL (play, renderSprite)
import Apecs.SDL.Internal (HSheet, Sheet (..), Texture, animate, linear, loadTexture, mkHSheet, mkRect, mkSheet)
import Control.Monad (forM_)
import Data.Either
import qualified Data.Map as Map
import qualified Env as Env
import Env (mkProp)
import Event (isKeyDown)
import GHC.TypeNats
import Linear (V2 (..))
import qualified SDL
import System.Component
import System.FilePath.Posix ((</>))
import System.World (System', World)

data Direction = North | East | South | West

data Action = Move Direction | Quit

initialize :: System' ()
initialize = do
  newEntity (CPlayer, CPosition (V2 0 0), CAnimation 0 0.5)
  pure ()

resources :: SDL.Renderer -> IO Env.Env
resources r = do
  prop <- mkProp <$> load "props.png"
  ground <- loadHSheet "ground.png"
  obstacle <- loadHSheet "obstacles.png"
  wall <- loadHSheet "obstacles.png"
  playerAttack <- loadHSheet "player_attack.png"
  playerIdle <- loadHSheet "player_idle.png"
  playerHurt <- loadHSheet "player_hurt.png"
  vampireIdle <- loadHSheet "vampire_idle.png"
  vampireAttack <- loadHSheet "vampire_attack.png"
  zombieIdle <- loadHSheet "zombie_idle.png"
  zombieAttack <- loadHSheet "zombie_attack.png"
  pure
    Env.Env
      { prop = prop,
        ground = Env.Ground ground,
        obstacle = Env.Obstacle obstacle,
        wall = Env.Wall wall,
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
    loadHSheet :: forall n. (KnownNat n, 1 <= n) => FilePath -> IO (HSheet n)
    loadHSheet f = mkHSheet @n <$> load f

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
draw Env.Env {player} r = do
  draws <- drawPlayer player
  lift draws
  where
    toScreen p = round . (* 32) <$> p
    render p = pure . renderSprite r (toScreen p)
    drawGround :: Env.Ground -> System' (IO ())
    drawGround Env.Ground {ground} = undefined
    drawPlayer :: Env.Player -> System' (IO ())
    drawPlayer Env.Player {idle} =
      cfoldM
        ( \_ (CPlayer, CPosition pos, CAnimation time dur) ->
            render pos (linear time dur idle)
        )
        mempty

step :: Env.Env -> Double -> [Action] -> System' World
step _ dt es = do
  cmap $ \(CPosition prev) -> Just (CPosition $ foldr update prev es)
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
