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
    Proxy (..),
    Set,
    ask,
    cfold,
    cfoldM,
    cfoldM_,
    cmap,
    cmapM,
    cmapM_,
    exists,
    get,
    global,
    lift,
    modify,
    newEntity,
    runWith,
    set,
  )
import Apecs.Experimental.Reactive
import Apecs.SDL (play, renderSprite)
import Apecs.SDL.Internal (ASheet, Sheet (..), Sprite, Texture, animate, linear, loadTexture, mkASheet, mkClips, mkRect, mkSheet)
import Control.Arrow
import Control.Monad (filterM, forM_, unless, void, when)
import qualified Control.Monad.State.Strict as State
import qualified Creature.Player as Player
import qualified Creature.Vampire as Vampire
import qualified Creature.Zombie as Zombie
import qualified Data.ByteString as ByteString
import Data.Either
import Data.Function
import Data.List
import Data.List.NonEmpty ((<|), NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Env
import Event (Event (..), events, isKeyDown)
import GHC.TypeNats
import Game.Component
import Game.World (All, System', World, initWorld)
import Linear (V2 (..))
import qualified SDL
import qualified SDL.Mixer
import System.FilePath.Posix ((</>))
import System.Random

toTime dt = floor (dt * 10000)

record :: Happened -> System' ()
record = records . (: [])

records :: [Happened] -> System' ()
records = modify global . (<>) . CLatest

tick :: Double -> System' ()
tick dt = do
  modify global $ \(CTime time) -> CTime (time + toTime dt)
  (CTime time) <- get global
  set global (CLatest [])

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

spread :: RandomGen r => Int -> r -> State.State [Position] [Position]
spread starts g = do
  positions <- State.get
  return (Set.toList . Set.fromList . concat $ start positions <$> gs)
  where
    decreaseChance = 0.1
    initialChance = 0.4
    gs = take starts $ iterate (snd . split) g
    start :: RandomGen r => [Position] -> r -> [Position]
    start positions g = expand g' initialChance n
      where
        ps = Set.fromList positions
        (n, g') = randPos g ps
        randPos :: RandomGen r => r -> Set.Set (Position) -> (Position, r)
        randPos g xs = let (n, g') = randomR (0, Set.size xs - 1) g in (Set.elemAt n xs, g')
        expand :: RandomGen r => r -> Double -> Position -> [Position]
        expand g chance n = n : concat (expand (snd . split $ g) (chance - decreaseChance) <$> neighbours)
          where
            neighbours =
              catMaybes
                . zipWith (\c p -> if c < chance then Just p else Nothing) (randomRs (0.0, 1.0) g)
                . filter (`Set.member` ps)
                $ (n +) <$> [V2 0 (-1), V2 0 1, V2 1 0, V2 (-1) 0]

initialize :: Word -> System' ()
initialize level = do
  set global (CLevel level)
  lift $ setStdGen (mkStdGen (fromIntegral level))
  g <- lift newStdGen
  let shrink n = (!! n) . iterate (tail . init)
      hedges = [V2 x y | x <- xs, y <- [head ys, last ys]]
      vedges = [V2 x y | x <- [head xs, last xs], y <- shrink 1 ys]
      ground = [V2 x y | x <- shrink 1 xs, y <- shrink 1 ys]
      spawnArea = [V2 x y | x <- shrink 2 xs, y <- shrink 2 ys]
      [zombies, vampires, sodas, fruit, obstacles] =
        flip State.evalState spawnArea $
          sequence [pick g 5, pick g 3, pick g 3, pick g 3, spread 10 g]
  newEnumsAt (randoms @Ground g) ground
  newEnumsAt (randoms @Wall g) (hedges ++ vedges)
  newEnumsAt (randoms @Obstacle g) obstacles
  sequence_ $ Zombie.new <$> zombies
  sequence_ $ Vampire.new <$> vampires
  Player.new (V2 1 (last ys - 1))
  let props =
        (Exit, V2 (last xs - 1) 1)
          : zip (repeat Fruit) fruit
          ++ zip (repeat Soda) sodas
  newProps props
  where
    newProps :: [(Prop, Position)] -> System' ()
    newProps = mapM_ go
      where
        go :: (Prop, Position) -> System' ()
        go (p, pos) =
          let new :: forall c. (Set World IO c) => c -> System' ()
              new c = void $ newEntity (c, CPosition pos, Clip p)
           in case p of
                Exit -> new CGoal
                Fruit -> new CFruit
                Soda -> new CSoda
    newEnumsAt :: (Set World IO (Clip a), Get World IO EntityCounter) => [a] -> [Position] -> System' ()
    newEnumsAt es ps = sequence_ $ newEntity . (Clip *** CPosition) <$> zip es ps
    ends :: [Double] -> (Int, Int)
    ends l = (floor $ head l, floor $ last l)
    shuffle :: RandomGen r => r -> [a] -> [a]
    shuffle r = fmap snd . sortBy (compare `on` fst) . zip (randoms @Int r)
    pick :: RandomGen r => r -> Int -> State.State [a] [a]
    pick r n = do
      state <- State.get
      let (picked, rest) = splitAt n (shuffle r state)
      State.put rest
      return picked
    xs :: [Int]
    xs = [0 .. 19]
    ys :: [Int]
    ys = [0 .. 14]

dirToV2 :: Direction -> Position
dirToV2 = \case
  East -> V2 distance 0
  West -> V2 (- distance) 0
  South -> V2 0 distance
  North -> V2 0 (- distance)
  where
    distance = 1

hurt :: Entity -> Word -> System' ()
hurt e n =
  modify e $ \(CStat Stat {hitpoints}) -> CStat Stat {hitpoints = hitpoints - fromIntegral n}

recover :: Entity -> Word -> System' ()
recover e n =
  modify e $ \(CStat Stat {hitpoints}) -> CStat Stat {hitpoints = hitpoints + fromIntegral n}

hasAny :: forall c. Get World IO c => [Entity] -> System' Bool
hasAny = fmap (not . null) . filterM (`exists` Proxy @c)

getAny :: forall c. Get World IO c => [Entity] -> System' (Maybe Entity)
getAny = fmap listToMaybe . filterM (`exists` Proxy @c)

entitiesAt :: Position -> System' [Entity]
entitiesAt pos = withReactive $ ixLookup (CPosition pos)

stepPlayer :: Position -> System' ()
stepPlayer next =
  cmapM $ \(CPlayer state) -> do
    cmap $ \(CPlayer _) -> Just (CPlayer mempty)
    at <- entitiesAt next
    win <- hasAny @CGoal at
    attack <- getAny @CEnemy at
    occupied <- hasAny @(Not CGround, Not CSoda, Not CFruit) at
    when win $ record PlayerWin
    moved <-
      if not occupied
        then do
          record PlayerMove
          pure $ Right (CPosition next)
        else pure $ Left ()
    attacked <-
      case attack of
        Just enemy -> do
          hurt enemy 1
          records [PlayerAttack, EnemyHurt]
          pure $ Just (CPlayer [PAttack])
        Nothing -> pure $ Just (CPlayer [])
    pure (attacked, moved)

finishAnimation :: forall c d. (Members World IO c, Get World IO c, Set World IO d) => d -> System' ()
finishAnimation to = cmap $ \(_ :: c, CAnimation time duration) ->
  eitherIf (const $ time >= duration) to

stepAnimation :: Double -> System' ()
stepAnimation dt = do
  cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)
  cmap $
    \(CPlayer ps, CAnimation time duration) -> case (time >= duration, ps) of
      (True, a : rest@(b : _)) -> Right (CPlayer rest, Player.animate b)
      _ -> Left ()

killEnemies :: System' ()
killEnemies = cmapM $ \(CEnemy, CStat Stat {hitpoints}) ->
  if hitpoints > 0
    then pure $ Left ()
    else do
      record EnemyDie
      pure $ Right CDead

stepEnemies :: Map.Map Position Entity -> System' ()
stepEnemies targets =
  cmapM $ \(CEnemy, eitherEnemy :: Either CZombie CVampire, CPosition pos) -> do
    g <- lift newStdGen
    let (direction, _) = random g
        next = dirToV2 direction + pos
        state :: Zombie -> Vampire -> Either CZombie CVampire
        state z v = case eitherEnemy of
          Left _ -> Left $ CZombie z
          Right _ -> Right $ CVampire v
    at <- entitiesAt next
    playerAt <- getAny @CPlayer at
    occupied <- hasAny @(Not CPlayer, Not CGround) at
    case (playerAt, occupied) of
      (Just target, _) -> do
        hurt target 1
        modify target $ \(CPlayer state) -> CPlayer (PHurt : state)
        record PlayerHurt
        record EnemyAttack
        pure $ Right (state ZAttack VAttack)
      (_, True) -> pure $ Right (state ZIdle VIdle)
      _ -> pure $ Left (CPosition next, state ZIdle VIdle)

stepItems :: Map.Map Position Entity -> System' ()
stepItems pickers = do
  pick @CFruit FruitPicked 1
  pick @CSoda SodaPicked 2
  where
    pick :: forall c. (Members World IO c, Get World IO c) => Happened -> Word -> System' ()
    pick happening i = cmapM $ \(_ :: c, CPosition pos, self :: Entity) ->
      case Map.lookup pos pickers of
        Nothing -> pure $ Left ()
        Just picker -> do
          recover picker i
          record happening
          pure $ Right CDead

removeDead :: System' ()
removeDead = cmap $ \CDead -> Not @All

stepState :: System' ()
stepState = do
  cmap $ \(CPlayer state) -> let state' = reverse (PIdle : state) in Just (CPlayer state', Player.animate (head state'))
  cmap $ \(CZombie state) -> Just (Zombie.animate state)
  cmap $ \(CVampire state) -> Just (Vampire.animate state)

step :: Env.Env -> Double -> [Event] -> System' World
step _ dt events = do
  removeDead
  tick dt
  stepAnimation dt
  shouldUpdate <- evalNext events
  whenJust shouldUpdate $ \next -> do
    stepPlayer next
    killEnemies
    targets <- getEntities @CPlayer
    stepItems targets
    stepEnemies targets
    stepState
  win <- (\(CLatest latest) -> any isPlayerWin latest) <$> get global
  if not win
    then ask
    else do
      (CLevel level) <- get global
      lift $ do
        w <- initWorld
        runWith w (initialize (level + 1) >> ask)
  where
    getEntities :: forall c. (Members World IO c, Get World IO c) => System' (Map.Map Position Entity)
    getEntities = cfold (\acc (_ :: c, e, CPosition pos) -> Map.insert pos e acc) mempty

evalNext :: [Event] -> System' (Maybe Position)
evalNext events =
  flip cfoldM Nothing $
    \_ (CPlayer _, CPosition prev) ->
      pure (maybeIf (/= prev) $ move events prev)
  where
    move :: [Event] -> Position -> Position
    move = (+) . sum . mapMaybe dir
      where
        dir (InputMove d) = Just (dirToV2 d)
        dir _ = Nothing

whenHas :: forall c. (Members World IO c, Get World IO c) => System' () -> System' ()
whenHas op = do
  has <- cfold (\prev (_ :: c) -> prev || True) False
  when has op

isPlayerMove :: Happened -> Bool
isPlayerMove PlayerMove = True
isPlayerMove _ = False

isPlayerAttack :: Happened -> Bool
isPlayerAttack PlayerAttack = True
isPlayerAttack _ = False

isEnemyAttack :: Happened -> Bool
isEnemyAttack EnemyAttack = True
isEnemyAttack _ = False

isSodaPicked :: Happened -> Bool
isSodaPicked SodaPicked = True
isSodaPicked _ = False

isFruitPicked :: Happened -> Bool
isFruitPicked FruitPicked = True
isFruitPicked _ = False

isEnemyDie :: Happened -> Bool
isEnemyDie EnemyDie = True
isEnemyDie _ = False

isPlayerWin :: Happened -> Bool
isPlayerWin PlayerWin = True
isPlayerWin _ = False

draw :: Env.Env -> SDL.Renderer -> System' ()
draw Env.Env {player, vampire, zombie, ground, music, enemy, wall, obstacle, prop, misc} r = do
  isMusicPlay <- SDL.Mixer.playingMusic
  unless isMusicPlay (playMusic music)
  (CLatest latest) <- get global
  when (any isPlayerMove latest) (playFootstep player)
  when (any isPlayerAttack latest) (playChop player)
  when (any isEnemyAttack latest) (playEnemyAttack enemy)
  when (any isSodaPicked latest) (playSoda misc)
  when (any isFruitPicked latest) (playFruit misc)
  when (any isEnemyDie latest) (playEnemyDie enemy)
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
    playMusic :: ByteString.ByteString -> System' ()
    playMusic music = lift $ do
      SDL.Mixer.setMusicVolume 20
      decoded <- SDL.Mixer.decode music
      SDL.Mixer.playMusic SDL.Mixer.Once decoded
    playSound :: SDL.Mixer.Channel -> [SDL.Mixer.Chunk] -> System' ()
    playSound ch chunks = do
      g <- lift newStdGen
      let randomIndex = fst (randomR (0, length chunks - 1) g)
      SDL.Mixer.setVolume 20 (0 :: SDL.Mixer.Channel)
      void $ SDL.Mixer.playOn ch SDL.Mixer.Once (chunks !! randomIndex)
    playPlayerSound :: [SDL.Mixer.Chunk] -> System' ()
    playPlayerSound = playSound 0
    playMiscSound :: [SDL.Mixer.Chunk] -> System' ()
    playMiscSound = playSound 1
    playEnemySound :: [SDL.Mixer.Chunk] -> System' ()
    playEnemySound = playSound 2
    playFootstep :: Env.Player -> System' ()
    playFootstep Env.Player {sfxFootstep} = playPlayerSound sfxFootstep
    playChop :: Env.Player -> System' ()
    playChop Env.Player {sfxChop} = playPlayerSound sfxChop
    playEnemyAttack :: Env.Enemy -> System' ()
    playEnemyAttack Env.Enemy {sfxAttack} = playEnemySound [sfxAttack]
    playEnemyDie :: Env.Enemy -> System' ()
    playEnemyDie Env.Enemy {sfxDie} = playMiscSound [sfxDie]
    playSoda :: Env.Misc -> System' ()
    playSoda Env.Misc {sfxSoda} = playMiscSound sfxSoda
    playFruit :: Env.Misc -> System' ()
    playFruit Env.Misc {sfxFruit} = playMiscSound sfxFruit
    toScreen :: Integral a => Position -> V2 a
    toScreen p = fromIntegral . (* 32) <$> p
    render :: forall s. (Sprite s) => (Position, s) -> IO ()
    render (p, a) = renderSprite r (toScreen p) a
    cdraw :: forall c s. (Sprite s, Members World IO c, Get World IO c) => (c -> (Position, s)) -> System' ()
    cdraw f = cfoldM (\acc c -> pure (acc >> render (f c))) mempty >>= lift
    drawSheet :: forall c. (Members World IO (Clip c), Get World IO (Clip c), Ord c) => Sheet c -> System' ()
    drawSheet s = cdraw $ \(Clip c, CPosition pos, _ :: Not CDead) ->
      (pos, (s {clip = Just c}))
    drawAnimation :: forall c. (Members World IO c, Get World IO c) => (Position -> Double -> Double -> c -> IO ()) -> System' ()
    drawAnimation f = cfoldM (\acc (c :: c, CPosition pos, _ :: Not CDead, CAnimation time dur) -> pure (acc >> f pos time dur c)) mempty >>= lift
    drawPlayer :: Env.Player -> System' ()
    drawPlayer Env.Player {idle, hurt, attack} = drawAnimation @CPlayer $
      \pos time dur (CPlayer p) ->
        let render' :: forall n. (KnownNat n, 1 <= n) => ASheet n -> IO ()
            render' = render . (pos,) . linear time dur
         in case listToMaybe p of
              (Just PIdle) -> render' idle
              (Just PHurt) -> render' hurt
              (Just PAttack) -> render' attack
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
  w' <- runWith w (initialize 0 >> ask)
  play w' Env.resources events step draw
