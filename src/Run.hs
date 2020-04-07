{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Run where

import Apecs (EntityCounter, Get, Has, Members, Set, ask, cfold, cfoldM, cmap, lift, newEntity, runWith)
import Apecs.SDL (play, renderSprite)
import Apecs.SDL.Internal (ASheet, Sheet (..), Sprite, Texture, animate, linear, loadTexture, mkASheet, mkClips, mkRect, mkSheet)
import Control.Arrow
import Control.Monad (forM_)
import Data.Either
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
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

spread :: RandomGen r => [V2 Double] -> Int -> r -> [V2 Double]
spread positions starts g = Set.toList . Set.fromList . concat $ start <$> gs
  where
    decreaseChance = 0.1
    initialChance = 0.4
    ps = Set.fromList positions
    gs = take starts $ iterate (snd . split) g
    randPos :: RandomGen r => r -> Set.Set (V2 Double) -> (V2 Double, r)
    randPos g xs = let (n, g') = randomR (0, Set.size xs - 1) g in (Set.elemAt n xs, g')
    expand :: RandomGen r => r -> Double -> V2 Double -> [V2 Double]
    expand g chance n = n : concat (expand (snd . split $ g) (chance - decreaseChance) <$> neighbours)
      where
        neighbours =
          catMaybes
            . zipWith (\c p -> if c < chance then Just p else Nothing) (randomRs (0.0, 1.0) g)
            . filter (`Set.member` ps)
            $ (n +) <$> [V2 0 (-1), V2 0 1, V2 1 0, V2 (-1) 0]
    start :: RandomGen r => r -> [V2 Double]
    start g = expand g' initialChance n
      where
        (n, g') = randPos g ps

initialize :: System' ()
initialize = do
  let (g1 : g2 : g3 : g4 : g5 : g6 : g7 : _) = iterate (snd . split) (mkStdGen 1)
      shrink n = (!! n) . iterate (tail . init)
      hedges = [V2 x y | x <- xs, y <- [head ys, last ys]]
      vedges = [V2 x y | x <- [head xs, last xs], y <- shrink 1 ys]
      ground = [V2 x y | x <- shrink 1 xs, y <- shrink 1 ys]
      spawnArea = [V2 x y | x <- shrink 2 xs, y <- shrink 2 ys]
      (zombies, rest) = pick g5 5 spawnArea
      (vampires, rest') = pick g7 3 rest
      obstacles = spread rest' 10 g4
  newEnumsAt (randoms @Ground g1) ground
  newEnumsAt (randoms @Wall g2) (hedges ++ vedges)
  newEnumsAt (randoms @Obstacle g3) obstacles
  sequence_ $ newEntity . (CEnemy,CZombie,CAnimation 0 0.8,) . CPosition <$> zombies
  sequence_ $ newEntity . (CEnemy,CVampire,CAnimation 0 0.5,) . CPosition <$> vampires
  newEntity (CPlayer, CPosition (V2 1 (last ys - 1)), CAnimation 0 0.5)
  newEntity (CGoal, CPosition (V2 (last xs - 1) 1), Clip Exit)
  pure ()
  where
    newEnumsAt :: (Set World IO (Clip a), Get World IO EntityCounter) => [a] -> [Position] -> System' ()
    newEnumsAt es ps = sequence_ $ newEntity . (Clip *** CPosition) <$> zip es ps
    ends :: [Double] -> (Int, Int)
    ends l = (floor $ head l, floor $ last l)
    shuffle :: RandomGen r => r -> [a] -> [a]
    shuffle r = fmap snd . sortBy (compare `on` fst) . zip (randoms @Int r)
    pick :: RandomGen r => r -> Int -> [a] -> ([a], [a])
    pick r n xs = splitAt n (shuffle r xs)
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
draw Env.Env {player, vampire, zombie, ground, wall, obstacle, prop} r = do
  draws <- sequence [drawSheet ground, drawSheet wall, drawSheet obstacle, drawSheet prop, drawPlayer player, drawVampire vampire, drawZombie zombie]
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
    drawAnimation :: forall c n. (KnownNat n, 1 <= n, Members World IO c, Get World IO c) => ASheet n -> System' (IO ())
    drawAnimation sheet = cdraw $ \(_ :: c, CPosition pos, CAnimation time dur) -> (pos, linear time dur sheet)
    drawPlayer :: Env.Player -> System' (IO ())
    drawPlayer Env.Player {idle} = drawAnimation @CPlayer idle
    drawVampire :: Env.Vampire -> System' (IO ())
    drawVampire Env.Vampire {idle} = drawAnimation @CVampire idle
    drawZombie :: Env.Zombie -> System' (IO ())
    drawZombie Env.Zombie {idle} = drawAnimation @CZombie idle

step :: Env.Env -> Double -> [Action] -> System' World
step _ dt es = do
  walls <- cfold (getPositions @CWall) mempty
  obstacles <- cfold (getPositions @CObstacle) mempty
  let occupied = Set.union walls obstacles
  cmap $ \(CPlayer, CPosition prev) -> let next = foldr update prev es in Just . CPosition $ if Set.member next occupied then prev else next
  cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)
  ask
  where
    getPositions :: forall c. (Members World IO c, Get World IO c) => Set.Set Position -> (c, CPosition) -> Set.Set Position
    getPositions acc (_, CPosition pos) = Set.insert pos acc
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
