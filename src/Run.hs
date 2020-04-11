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
import Control.Monad (forM_, when)
import Data.Either
import Data.Function
import Data.List
import Data.List.NonEmpty ((<|), NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
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
import qualified SDL.Mixer
import System.FilePath.Posix ((</>))
import System.Random

data Event = InputMove Direction | InputQuit

send :: Entity -> Action -> System' ()
send e action = modify e $ \(CActionStream (as :| rest)) -> CActionStream ((action : as) :| rest)

sends :: Entity -> [Action] -> System' ()
sends e actions = modify e $ \(CActionStream (as :| rest)) -> CActionStream ((actions ++ as) :| rest)

tick :: Entity -> System' ()
tick e = modify e $ \(CActionStream stream@(as :| rest)) -> CActionStream ([] <| stream)

tickem :: forall c. (Members World IO c, Get World IO c) => System' ()
tickem = cmap $ \(_ :: c, CActionStream stream@(as :| rest)) -> Just (CActionStream ([] <| stream))

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

eitherIf :: (a -> Bool) -> a -> Either () a
eitherIf f a = if f a then Right a else Left ()

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
  sfxFootstep1 <- loadAudio "scavengers_footstep1.aif"
  sfxFootstep2 <- loadAudio "scavengers_footstep1.aif"
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
              hurt = playerHurt,
              sfxFootstep = [sfxFootstep1, sfxFootstep2]
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
    loadAudio :: FilePath -> IO SDL.Mixer.Chunk
    loadAudio f = SDL.Mixer.load ("resources" </> "audio" </> f)
    loadImage :: FilePath -> IO Texture
    loadImage f = loadTexture r ("resources" </> "sprites" </> f)
    loadSheet32x32 :: forall a. (Enum a, Ord a, Bounded a) => FilePath -> IO (Sheet a)
    loadSheet32x32 f = (`mkSheet` mkClips (32, 32)) <$> loadImage f
    loadASheet32x32 :: forall n. (KnownNat n, 1 <= n) => FilePath -> IO (ASheet n)
    loadASheet32x32 f = (`mkASheet` (32, 32)) <$> loadImage f

movement :: Either Event SDL.Event -> Either Event SDL.Event
movement e = do
  payload <- SDL.eventPayload <$> e
  let pressed c r = if isKeyDown c payload then Left r else pure ()
  pressed SDL.ScancodeRight (InputMove East)
  pressed SDL.ScancodeLeft (InputMove West)
  pressed SDL.ScancodeUp (InputMove North)
  pressed SDL.ScancodeDown (InputMove South)
  e

dirToV2 :: Direction -> Position
dirToV2 = \case
  East -> V2 distance 0
  West -> V2 (- distance) 0
  South -> V2 0 distance
  North -> V2 0 (- distance)
  where
    distance = 1

peekActions :: (Action -> Maybe a) -> Entity -> System' [a]
peekActions f e = do
  actions <- unwrap <$> get e
  pure $ catMaybes (f <$> actions)
  where
    unwrap (CActionStream actions) = NonEmpty.head actions

sumHurt :: Entity -> System' Word
sumHurt = fmap sum . peekActions sieveHurt
  where
    sieveHurt (Hurt d) = Just d
    sieveHurt _ = Nothing

sumMove :: Entity -> System' (V2 Double)
sumMove = fmap sum . peekActions sieveMove
  where
    sieveMove (Move d) = Just (dirToV2 d)
    sieveMove _ = Nothing

playerAttack :: Map.Map Position Entity -> (Position -> Maybe Position) -> System' ()
playerAttack enemies move = cmapM_ $ \(CPlayer p, CPosition pos, e :: Entity) ->
  whenJust (move pos) $ \next ->
    case (p, Map.lookup next enemies) of
      (PAttack, _) -> pure ()
      (_, Nothing) -> pure ()
      (_, Just enemy) -> do
        send enemy (Hurt 1)
        set e (CPlayer PAttack, Player.attack)

playerMove :: Set.Set Position -> (Position -> Maybe Position) -> System' ()
playerMove occupied move = cmapM_ $ \(CPlayer _, CPosition pos, e :: Entity) ->
  whenJust (move pos) $ \next ->
    when (Set.notMember next occupied) $ set e (CPlayer PIdle, Player.idle, CPosition next)

finishAnimation :: forall c d. (Members World IO c, Get World IO c, Set World IO d) => d -> System' ()
finishAnimation to = cmap $ \(_ :: c, CAnimation time duration) ->
  eitherIf (const $ time >= duration) to

playerAnimate :: System' ()
playerAnimate = finishAnimation @CPlayer (CPlayer PIdle, Player.idle)

playerCollide :: Set.Set Position -> System' ()
playerCollide occupied = cmapM_ $ \(CPlayer _, CPosition pos, e :: Entity) ->
  let directions = find (`Set.notMember` occupied) $ (+ pos) . dirToV2 <$> [North, East, South, West]
   in when (Set.member pos occupied) . whenJust directions $ \destination ->
        set e (CPosition destination)

playerHurt :: System' ()
playerHurt = cmapM $ \(CPlayer _, CStat Stat {hitpoints}, e :: Entity) -> do
  damage <- sumHurt e
  pure $
    eitherIf (const $ damage /= 0) (CPlayer PHurt, CStat Stat {hitpoints = hitpoints - fromIntegral damage})

stepPlayer :: Set.Set Position -> Map.Map Position Entity -> System' ()
stepPlayer occupied enemies =
  cmapM_ $ \(CPlayer p, e :: Entity) -> do
    offset <- sumMove e
    let next pos = maybeIf (/= pos) (pos + offset)
    playerCollide occupied
    playerHurt
    playerAttack enemies next
    playerMove occupied next
    tick e

stepAnimation :: Double -> System' ()
stepAnimation dt = do
  cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)
  zombieAnimate
  vampireAnimate
  playerAnimate

enemiesDie :: System' ()
enemiesDie = cmap $ \(CEnemy, CStat Stat {hitpoints}) ->
  eitherIf (const $ hitpoints <= 0) (Not @(CEnemy, CAnimation, CZombie, CVampire, CPosition, CStat, CActionStream))

enemiesHurt :: System' ()
enemiesHurt = cmapM $ \(CEnemy, CStat Stat {hitpoints}, e :: Entity) -> do
  damage <- sumHurt e
  pure (CStat Stat {hitpoints = hitpoints - fromIntegral damage})

enemiesAttackMove :: forall c. Get World IO c => (Entity -> System' ()) -> Position -> Set.Set Position -> System' (Set.Set Position)
enemiesAttackMove onAttack playerPosition = cfoldM $ \occupied (CEnemy, CPosition pos, _ :: c, e :: Entity) -> do
  g <- lift newStdGen
  let (direction, _) = random g
      next = dirToV2 direction + pos
  case (Set.member next occupied, playerPosition == next) of
    (False, f) -> do
      when f (onAttack e)
      set e (CPosition next)
      pure (Set.insert next . Set.delete pos $ occupied)
    (True, _) -> pure occupied

zombieAnimate :: System' ()
zombieAnimate = finishAnimation @CZombie (CZombie ZIdle, Zombie.idle)

vampireAnimate :: System' ()
vampireAnimate = finishAnimation @CVampire (CVampire VIdle, Vampire.idle)

stepEnemies :: Set.Set Position -> Bool -> System' ()
stepEnemies occupied shouldUpdate =
  when shouldUpdate . cmapM_ $ \(CPlayer _, CPosition playerPos, player :: Entity) -> do
    let attackMove :: forall c. Get World IO c => (Entity -> System' ()) -> Set.Set Position -> System' (Set.Set Position)
        attackMove f = enemiesAttackMove @c f playerPos
        zombieAttackMove = attackMove @CZombie $ \zombie -> do
          set zombie (CZombie ZAttack, Zombie.attack)
          send player (Hurt 2)
        vampireAttackMove = attackMove @CVampire $ \vampire -> do
          set vampire (CVampire VAttack, Vampire.attack)
          send player (Hurt 1)
    vampireAttackMove occupied
      >>= zombieAttackMove
    enemiesHurt
    enemiesDie
    tickem @CEnemy

toActions :: [Event] -> [Action]
toActions = mapMaybe toAction
  where
    toAction (InputMove d) = Just (Move d)
    toAction _ = Nothing

stepEvents :: [Event] -> System' ()
stepEvents events = cmapM_ $ \(CPlayer _, e :: Entity) -> sends e (toActions events)

events :: Env.Env -> [SDL.Event] -> [Event]
events _ es = lefts $ movement . Right <$> es

step :: Env.Env -> Double -> [Event] -> System' World
step _ dt actions = do
  walls <- getEntities @CWall
  obstacles <- getEntities @CObstacle
  enemies <- getEntities @CEnemy
  let occupied = foldl1 Set.union $ Map.keysSet <$> [walls, obstacles, enemies]
      shouldUpdate = not . null $ actions
  stepEvents actions
  stepAnimation dt
  stepPlayer occupied enemies
  stepEnemies occupied shouldUpdate
  ask
  where
    getEntities :: forall c. (Members World IO c, Get World IO c) => System' (Map.Map Position Entity)
    getEntities = cfold (\acc (_ :: c, e, CPosition pos) -> Map.insert pos e acc) mempty

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

run :: World -> IO ()
run w = do
  w' <- runWith w (initialize >> ask)
  play w' resources events step draw
