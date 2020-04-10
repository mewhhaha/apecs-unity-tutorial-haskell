{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Run where

import Apecs
  ( Entity (..),
    EntityCounter,
    Get,
    Has,
    Members,
    Not (..),
    Set,
    ask,
    cfold,
    cfoldM,
    cfoldM_,
    cmap,
    cmapM,
    cmapM_,
    get,
    lift,
    modify,
    newEntity,
    runWith,
    set,
  )
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

data Event = InputMove Direction | InputQuit

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

maybeIf :: (a -> Bool) -> a -> Maybe a
maybeIf f a = if f a then Just a else Nothing

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
  lift $ setStdGen (mkStdGen 0)
  g <- lift newStdGen
  let shrink n = (!! n) . iterate (tail . init)
      hedges = [V2 x y | x <- xs, y <- [head ys, last ys]]
      vedges = [V2 x y | x <- [head xs, last xs], y <- shrink 1 ys]
      ground = [V2 x y | x <- shrink 1 xs, y <- shrink 1 ys]
      spawnArea = [V2 x y | x <- shrink 2 xs, y <- shrink 2 ys]
      (zombies, rest) = pick g 5 spawnArea
      (vampires, rest') = pick g 3 rest
      obstacles = spread rest' 10 g
  newEnumsAt (randoms @Ground g) ground
  newEnumsAt (randoms @Wall g) (hedges ++ vedges)
  newEnumsAt (randoms @Obstacle g) obstacles
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

movement :: Either Event SDL.Event -> Either Event SDL.Event
movement e = do
  payload <- SDL.eventPayload <$> e
  let pressed c r = if isKeyDown c payload then Left r else pure ()
  pressed SDL.ScancodeRight (InputMove East)
  pressed SDL.ScancodeLeft (InputMove West)
  pressed SDL.ScancodeUp (InputMove North)
  pressed SDL.ScancodeDown (InputMove South)
  e

events :: Env.Env -> [SDL.Event] -> [Event]
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

move :: Direction -> Position
move = \case
  East -> V2 distance 0
  West -> V2 (- distance) 0
  South -> V2 0 distance
  North -> V2 0 (- distance)
  where
    distance = 1

consumeAction :: (Action -> Either a Action) -> Entity -> System' [a]
consumeAction f e = do
  (values, rest) <- partitionEithers . split <$> get e
  set e (CActions rest)
  return values
  where
    split (CActions actions) = f <$> actions

consumeHurt :: Entity -> System' Word
consumeHurt = fmap sum . consumeAction sieveHurt
  where
    sieveHurt (Hurt d) = Left d
    sieveHurt a = Right a

consumeMove :: Entity -> System' (V2 Double)
consumeMove = fmap sum . consumeAction sieveMove
  where
    sieveMove (Move d) = Left (move d)
    sieveMove a = Right a

playerAttack :: Map.Map Position Entity -> Maybe Position -> System' ()
playerAttack enemies = flip whenJust $
  \next -> cmapM $ \(CPlayer p) -> case (p, Map.lookup next enemies) of
    (PAttack, _) -> pure $ Left ()
    (_, Nothing) -> pure $ Left ()
    (_, Just enemy) -> do
      modify enemy (\(CActions actions) -> CActions (Hurt 1 : actions))
      pure $ Right (CPlayer PAttack, Player.attack)

playerMove :: Set.Set Position -> Maybe Position -> System' ()
playerMove occupied = flip whenJust $
  \next -> cmap $ \(CPlayer _) -> if Set.member next occupied then Left () else Right (CPlayer PIdle, Player.idle, CPosition next)

playerAnimate :: System' ()
playerAnimate = cmap $ \(CPlayer _, CAnimation time duration) ->
  if time <= duration
    then Left ()
    else Right (CPlayer PIdle, Player.idle)

playerHurt :: System' ()
playerHurt = cmapM $ \(CPlayer _, CStat Stat {hitpoints}, e :: Entity) -> do
  damage <- consumeHurt e
  pure $
    if damage == 0
      then Left ()
      else Right (CPlayer PHurt, CStat Stat {hitpoints = hitpoints - fromIntegral damage})

stepPlayer :: Set.Set Position -> Map.Map Position Entity -> System' ()
stepPlayer occupied enemies = cmapM_ $ \(CPlayer p, CActions actions, CPosition prev, e :: Entity) -> do
  offset <- consumeMove e
  let next = maybeIf (/= prev) (prev + offset)
  playerAttack enemies next
  playerHurt
  playerMove occupied next
  playerAnimate

stepAnimation :: Double -> System' ()
stepAnimation dt = cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)

enemiesDie :: System' ()
enemiesDie = cmap $ \(CEnemy, CStat Stat {hitpoints}) ->
  if hitpoints <= 0
    then Left $ Not @(CEnemy, CAnimation, CZombie, CVampire, CPosition, CStat, CActions)
    else Right ()

enemiesHurt :: System' ()
enemiesHurt = cmapM $ \(CEnemy, CStat Stat {hitpoints}, e :: Entity) -> do
  damage <- consumeHurt e
  pure (CStat Stat {hitpoints = hitpoints - fromIntegral damage})

enemiesMove :: Position -> Entity -> Set.Set Position -> System' ()
enemiesMove player playerEntity = cfoldM_ $ \occupied (CEnemy, CPosition pos, e :: Entity) -> do
  g <- lift newStdGen
  let (direction, _) = random g
      next = move direction + pos
  case (Set.member next occupied, player == next) of
    (_, True) -> do
      modify playerEntity (\(CActions actions) -> CActions (Hurt 1 : actions))
      pure occupied
    (True, _) -> pure occupied
    _ -> do
      set e (CPosition next)
      pure (Set.insert next . Set.delete pos $ occupied)

stepEnemies :: Set.Set Position -> Bool -> System' ()
stepEnemies occupied shouldUpdate =
  if not shouldUpdate
    then pure ()
    else cmapM_ $ \(CPlayer _, CPosition player, e :: Entity) -> do
      enemiesMove player e occupied
      enemiesHurt
      enemiesDie
      pure ()

toActions :: [Event] -> [Action]
toActions = mapMaybe toAction
  where
    toAction (InputMove d) = Just (Move d)
    toAction _ = Nothing

stepEvents :: [Event] -> System' ()
stepEvents events = cmap $ \(CPlayer _, CActions actions) -> Just (CActions (actions ++ toActions events))

step :: Env.Env -> Double -> [Event] -> System' World
step _ dt actions = do
  walls <- cfold (getPositions @CWall) mempty
  obstacles <- cfold (getPositions @CObstacle) mempty
  enemies <- cfold (getPositions @CEnemy) mempty
  let occupied = foldl1 Set.union $ Map.keysSet <$> [walls, obstacles, enemies]
  stepEvents actions
  stepAnimation dt
  stepPlayer occupied enemies
  stepEnemies occupied (not . null $ actions)
  ask
  where
    getPositions :: forall c. (Members World IO c, Get World IO c) => Map.Map Position Entity -> (c, Entity, CPosition) -> Map.Map Position Entity
    getPositions acc (_, e, CPosition pos) = Map.insert pos e acc

run :: World -> IO ()
run w = do
  w' <- runWith w (initialize >> ask)
  play w' resources events step draw
