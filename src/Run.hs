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
    global,
    lift,
    modify,
    newEntity,
    runWith,
    set,
  )
import Apecs.SDL (play, renderSprite)
import Apecs.SDL.Internal (ASheet, Sheet (..), Sprite, Texture, animate, linear, loadTexture, mkASheet, mkClips, mkRect, mkSheet)
import Control.Arrow
import Control.Monad (forM_, void, when)
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

toTime dt = floor (dt * 10000)

send :: Entity -> Action -> System' ()
send e action = modify e $ \(CActionStream ((t, as) :| rest)) -> CActionStream ((t, action : as) :| rest)

sends :: Entity -> [Action] -> System' ()
sends e actions = modify e $ \(CActionStream ((t, as) :| rest)) -> CActionStream ((t, actions ++ as) :| rest)

tick :: Double -> System' ()
tick dt = do
  modify global $ \(CTime time) -> (CTime (time + toTime dt))
  (CTime time) <- get global
  cmap $ \(CActionStream stream@(as :| rest)) ->
    let next = (time, [])
     in Just $ CActionStream (if null as then next :| rest else next <| stream)

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

whenNothing :: Applicative m => Maybe a -> m () -> m ()
whenNothing Nothing f = f
whenNothing (Just _) _ = pure ()

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
  SDL.Mixer.setVolume 20 sfxFootstep1
  SDL.Mixer.setVolume 20 sfxFootstep2
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
  (_, actions) <- unwrap <$> get e
  pure $ catMaybes (f <$> actions)
  where
    unwrap (CActionStream actions) = NonEmpty.head actions

sumHurt :: Entity -> System' Word
sumHurt = fmap sum . peekActions sieveHurt
  where
    sieveHurt (Hurt d) = Just d
    sieveHurt _ = Nothing

playerAttack :: Map.Map Position Entity -> Position -> System' ()
playerAttack enemies next = cmapM_ $ \(CPlayer p, CPosition pos, player :: Entity) ->
  case (p, Map.lookup next enemies) of
    (PAttack, _) -> pure ()
    (_, Nothing) -> pure ()
    (_, Just enemy) -> do
      send enemy (Hurt 1)
      send player Attack

playerMove :: Set.Set Position -> Position -> System' ()
playerMove occupied next = cmapM_ $ \(CPlayer _, CPosition pos, player :: Entity) ->
  when (Set.notMember next occupied) $ send player (Movement next)

finishAnimation :: forall c d. (Members World IO c, Get World IO c, Set World IO d) => d -> System' ()
finishAnimation to = cmap $ \(_ :: c, CAnimation time duration) ->
  eitherIf (const $ time >= duration) to

stepAnimation :: Double -> System' ()
stepAnimation dt =
  cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)

killEnemies :: System' ()
killEnemies = cmap $ \(CEnemy, CStat Stat {hitpoints}) ->
  eitherIf (const $ hitpoints <= 0) (Not @(CEnemy, CAnimation, CZombie, CVampire, CPosition, CStat, CActionStream))

evalEnemies :: Map.Map Position Entity -> Set.Set Position -> System' ()
evalEnemies targets =
  cfoldM_ $ \occupied (CEnemy, CPosition pos, enemy :: Entity) -> do
    g <- lift newStdGen
    let (direction, _) = random g
        next = dirToV2 direction + pos
        unchanged = pure occupied
    case (Set.member next occupied, Map.lookup next targets) of
      (_, Just target) -> do
        send target (Hurt 1)
        send enemy Attack
        unchanged
      (True, _) -> unchanged
      _ -> do
        send enemy (Movement next)
        pure (Set.insert next . Set.delete pos $ occupied)

evalEvents :: [Event] -> Map.Map Position Entity -> Set.Set Position -> System' ()
evalEvents events enemies occupied =
  cmapM_ $ \(CPlayer _, CPosition prev, player :: Entity) ->
    whenJust (maybeIf (/= prev) $ move events prev) $ \next ->
      case (Set.member next occupied, Map.lookup next enemies) of
        (_, Just enemy) -> do
          send enemy (Hurt 1)
          send player Attack
        (True, _) -> pure ()
        _ -> send player (Movement next)
  where
    move :: [Event] -> Position -> Position
    move = (+) . sum . mapMaybe dir
      where
        dir (InputMove d) = Just (dirToV2 d)
        dir _ = Nothing

events :: Env.Env -> [SDL.Event] -> [Event]
events _ es = lefts $ movement . Right <$> es

peekLatest :: forall c d. (Members World IO c, Get World IO c, Set World IO d) => ([Action] -> d) -> System' ()
peekLatest sys = cmap $ \(_ :: c, CActionStream stream) ->
  let latest = snd . NonEmpty.head $ stream
   in sys latest

moveLatest :: forall c. (Members World IO c, Get World IO c) => System' ()
moveLatest = peekLatest @c $ \latest ->
  case listToMaybe . mapMaybe dests $ latest of
    Nothing -> Left ()
    Just dest -> Right (CPosition dest)
  where
    dests (Movement dest) = Just dest
    dests _ = Nothing

movePlayer :: System' ()
movePlayer = moveLatest @CPlayer

moveEnemies :: System' ()
moveEnemies = moveLatest @CEnemy

hurtLatest :: forall c. (Members World IO c, Get World IO c) => System' ()
hurtLatest = cmap $ \(_ :: c, CStat Stat {hitpoints}, CActionStream stream) ->
  let latest = snd . NonEmpty.head $ stream
   in case maybeIf (/= 0) (sum . mapMaybe hurts $ latest) of
        Nothing -> Left ()
        Just damage -> Right (CStat Stat {hitpoints = hitpoints - fromIntegral damage})
  where
    hurts (Hurt damage) = Just damage
    hurts _ = Nothing

hurtEnemies :: System' ()
hurtEnemies = hurtLatest @CEnemy

hurtPlayer :: System' ()
hurtPlayer = hurtLatest @CPlayer

statePlayer :: System' ()
statePlayer = peekLatest @CPlayer $ \latest -> case listToMaybe . sort $ latest of
  Just (Hurt _) -> Right (CPlayer PHurt, Player.hurt)
  Just Attack -> Right (CPlayer PAttack, Player.attack)
  Just (Movement _) -> Right (CPlayer PIdle, Player.idle)
  _ -> Left ()

stateZombie :: System' ()
stateZombie = peekLatest @CZombie $ \latest -> case listToMaybe . sort $ latest of
  Just Attack -> Right (CZombie ZAttack, Zombie.attack)
  Just (Movement _) -> Right (CZombie ZIdle, Zombie.idle)
  _ -> Left ()

stateVampire :: System' ()
stateVampire = peekLatest @CVampire $ \latest -> case listToMaybe . sort $ latest of
  Just Attack -> Right (CVampire VAttack, Vampire.attack)
  Just (Movement _) -> Right (CVampire VIdle, Vampire.idle)
  _ -> Left ()

step :: Env.Env -> Double -> [Event] -> System' World
step _ dt actions = do
  tick dt
  stepAnimation dt
  let shouldUpdate = not . null $ actions
  when shouldUpdate $ do
    walls <- getEntities @CWall
    obstacles <- getEntities @CObstacle
    enemies <- getEntities @CEnemy
    let occupied = foldl1 Set.union $ Map.keysSet <$> [walls, obstacles, enemies]
    evalEvents actions enemies occupied
    hurtEnemies
    killEnemies
    movePlayer
    player <- getEntities @CPlayer
    evalEnemies player occupied
    hurtPlayer
    moveEnemies
    statePlayer
    stateZombie
    stateVampire
  ask
  where
    getEntities :: forall c. (Members World IO c, Get World IO c) => System' (Map.Map Position Entity)
    getEntities = cfold (\acc (_ :: c, e, CPosition pos) -> Map.insert pos e acc) mempty

isMovement :: Action -> Bool
isMovement (Movement _) = True
isMovement _ = False

whenMove :: [Action] -> System' () -> System' ()
whenMove as op = if has then op else pure ()
  where
    has = any isMovement as

draw :: Env.Env -> SDL.Renderer -> System' ()
draw Env.Env {player, vampire, zombie, ground, wall, obstacle, prop} r = do
  cmapM_ $ \(_ :: CPlayer, CActionStream stream) ->
    let latest = snd . NonEmpty.head $ stream
     in whenMove latest (playFootstep player)
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
    playFootstep :: Env.Player -> System' ()
    playFootstep Env.Player {sfxFootstep} = do
      g <- lift newStdGen
      let randomIndex = fst (randomR (0, length sfxFootstep - 1) g)
      SDL.Mixer.playOn 0 SDL.Mixer.Once (sfxFootstep !! randomIndex)
      pure ()
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
