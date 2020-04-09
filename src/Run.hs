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

import Apecs (Entity (..), EntityCounter, Get, Has, Members, Not (..), Set, ask, cfold, cfoldM, cmap, cmapM, lift, modify, newEntity, runWith)
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
import qualified Enemy.Vampire as Vampire
import qualified Enemy.Zombie as Zombie
import qualified Env
import Event (isKeyDown)
import GHC.TypeNats
import Game.Component
import Game.World (System', World)
import Linear (V2 (..))
import qualified Player.Player as Player
import qualified SDL
import System.FilePath.Posix ((</>))
import System.Random

data Direction = North | East | South | West

data Action = Move Direction | Quit

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

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
  sequence_ $ Zombie.new <$> zombies
  sequence_ $ Vampire.new <$> vampires
  Player.new (V2 1 (last ys - 1))
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
draw Env.Env {player, vampire, zombie, ground, wall, obstacle, prop} r =
  sequence_
    [ drawSheet ground,
      drawSheet wall,
      drawSheet obstacle,
      drawSheet prop,
      drawPlayer player,
      drawVampire vampire,
      drawZombie zombie
    ]
  where
    toScreen :: Integral a => V2 Double -> V2 a
    toScreen p = round . (* 32) <$> p
    render :: forall s. (Sprite s) => (V2 Double, s) -> IO ()
    render (p, a) = renderSprite r (toScreen p) a
    cdraw :: forall c s. (Sprite s, Members World IO c, Get World IO c) => (c -> (V2 Double, s)) -> System' ()
    cdraw f = cfoldM (\acc c -> pure (acc >> render (f c))) mempty >>= lift
    drawSheet :: forall c. (Members World IO (Clip c), Get World IO (Clip c), Ord c) => Sheet c -> System' ()
    drawSheet s = cdraw $ \(Clip c, CPosition pos) ->
      (pos, (s {clip = Just c}))
    drawAnimation :: forall c. (Members World IO c, Get World IO c) => (V2 Double -> Double -> Double -> c -> IO ()) -> System' ()
    drawAnimation f = cfoldM (\acc (c :: c, CPosition pos, CAnimation time dur) -> pure (acc >> f pos time dur c)) mempty >>= lift
    drawPlayer :: Env.Player -> System' ()
    drawPlayer Env.Player {idle, hurt, attack} = drawAnimation @CPlayer $
      \pos time dur (CPlayer p) ->
        let render' :: forall n. (KnownNat n, 1 <= n) => ASheet n -> IO ()
            render' = render . (pos,) . linear time dur
         in case p of
              PIdle -> render' idle
              PHurt -> render' hurt
              PAttack -> render' attack
    drawVampire :: Env.Vampire -> System' ()
    drawVampire Env.Vampire {idle, attack} = drawAnimation @CVampire $
      \pos time dur (CVampire p) ->
        let render' :: forall n. (KnownNat n, 1 <= n) => ASheet n -> IO ()
            render' = render . (pos,) . linear time dur
         in case p of
              VIdle -> render' idle
              VAttack -> render' attack
    drawZombie :: Env.Zombie -> System' ()
    drawZombie Env.Zombie {idle, attack} = drawAnimation @CZombie $
      \pos time dur (CZombie p) ->
        let render' :: forall n. (KnownNat n, 1 <= n) => ASheet n -> IO ()
            render' = render . (pos,) . linear time dur
         in case p of
              ZIdle -> render' idle
              ZAttack -> render' attack

move :: Action -> Position -> Position
move (Move m) p = p + case m of
  East -> V2 distance 0
  West -> V2 (- distance) 0
  South -> V2 0 distance
  North -> V2 0 (- distance)
  where
    distance = 1
move _ p = p

playerAttack :: Map.Map Position Entity -> Maybe Position -> System' ()
playerAttack enemies = flip whenJust $
  \next -> cmapM $ \(CPlayer p, CPosition prev) -> case (p, Map.lookup next enemies) of
    (PAttack, _) -> pure $ Left ()
    (_, Nothing) -> pure $ Left ()
    (_, Just enemy) -> do
      modify enemy (\(CEnemy Enemy {hitpoints}) -> CEnemy (Enemy {hitpoints = hitpoints - 1}))
      pure $ Right (CPlayer PAttack, Player.attack)

playerMove :: Map.Map Position a -> Maybe Position -> System' ()
playerMove occupied = flip whenJust $
  \next -> cmap $ \(CPlayer p, CPosition prev) ->
    if prev == next || Map.member next occupied then Left () else Right (CPlayer PIdle, Player.idle, CPosition next)

playerAnimate :: System' ()
playerAnimate = cmap $ \(CPlayer p, CAnimation time duration) ->
  if time <= duration
    then Left ()
    else Right (CPlayer PIdle, Player.idle)

stepAnimation :: Double -> System' ()
stepAnimation dt = cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)

killEnemies :: System' ()
killEnemies = cmap $ \(CEnemy Enemy {hitpoints}) -> case hitpoints of
  0 -> Left $ Not @(CEnemy, CAnimation, CZombie, CVampire, CPosition)
  _ -> Right ()

step :: Env.Env -> Double -> [Action] -> System' World
step _ dt as = do
  walls <- cfold (getPositions @CWall) mempty
  obstacles <- cfold (getPositions @CObstacle) mempty
  enemies <- cfold (getPositions @CEnemy) mempty
  let occupied = walls `Map.union` obstacles `Map.union` enemies
  next <- listToMaybe <$> cfold (\_ (CPlayer _, CPosition pos) -> [foldr move pos as]) []
  playerAttack enemies next
  playerMove occupied next
  playerAnimate
  killEnemies
  stepAnimation dt
  ask
  where
    getPositions :: forall c. (Members World IO c, Get World IO c) => Map.Map Position Entity -> (c, Entity, CPosition) -> Map.Map Position Entity
    getPositions acc (_, e, CPosition pos) = Map.insert pos e acc

run :: World -> IO ()
run w = do
  w' <- runWith w (initialize >> ask)
  play w' resources events step draw
