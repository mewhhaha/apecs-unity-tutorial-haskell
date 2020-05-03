{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Apecs.SDL (play, render)
import Apecs.SDL.Internal (ASheet, Drawable, Sheet (..), Texture, animate, linear, loadTexture, mkASheet, mkClips, mkPoint, mkRect, mkSheet, mkTextElement)
import Control.Arrow
import Control.Monad (filterM, forM_, join, unless, void, when, zipWithM_)
import Control.Monad.IO.Class (MonadIO)
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
import qualified Data.StateVar as StateVar
import qualified Data.Text as Text
import qualified Env
import Event (Event (..), events, isKeyDown)
import GHC.TypeNats
import Game.Component
import Game.World (All, System', World, initWorld)
import Linear ((*^), V2 (..), (^+^), (^-^))
import qualified SDL
import qualified SDL.Mixer
import qualified SDL.Video
import System.FilePath.Posix ((</>))
import System.Random

whenM :: Monad m => (m Bool) -> m () -> m ()
whenM condition op = join ((`when` op) <$> condition)

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
        randPos :: RandomGen r => r -> Set.Set Position -> (Position, r)
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
      start = V2 1 (last ys - 1)
      goal = V2 (last xs - 1) 1
      spawnArea = [V2 x y | x <- shrink 1 xs, y <- shrink 1 ys, notElem (V2 x y) [start, goal]]
      [zombies, vampires, sodas, fruit, obstacles] =
        flip State.evalState spawnArea $
          sequence [pick g 5, pick g 3, pick g 3, pick g 3, spread 10 g]
  newEnumsAt (randoms @Ground g) ground
  newEnumsAt (randoms @Wall g) (hedges ++ vedges)
  newObstacles (randoms @Obstacle g) obstacles
  mapM_ Zombie.new zombies
  mapM_ Vampire.new vampires
  Player.new start
  let props =
        (Exit, goal)
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
    newObstacles :: [Obstacle] -> [Position] -> System' ()
    newObstacles = zipWithM_ go
      where
        go v p = void $ newEntity (CPosition p, CObstacle v, CStat Stat {life = 2})
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
  modify e $ \(CStat Stat {life}) -> CStat Stat {life = life - fromIntegral n}

recover :: Entity -> Word -> System' ()
recover e n =
  modify e $ \(CStat Stat {life}) -> CStat Stat {life = life + fromIntegral n}

hasAny :: forall c. Get World IO c => [Entity] -> System' Bool
hasAny = fmap (not . null) . filterM (`exists` Proxy @c)

getAny :: forall c. Get World IO c => [Entity] -> System' (Maybe Entity)
getAny = fmap listToMaybe . filterM (`exists` Proxy @c)

entitiesAt :: Position -> System' [Entity]
entitiesAt pos = withReactive $ ixLookup (CPosition pos)

stepPlayer :: Position -> System' ()
stepPlayer next =
  cmapM $ \(CPlayer state, CPosition pos, CStat Stat {life}) -> do
    at <- entitiesAt next
    win <- hasAny @CGoal at
    attack <- getAny @(Not CPlayer, CStat) at
    occupied <- hasAny @(Not CGround, Not CSoda, Not CFruit) at
    when win $ record PlayerWin
    moving <-
      if not occupied
        then do
          record PlayerMove
          pure $ Right (CPosition next, CLinear (Linear 0 pos next))
        else pure $ Left ()
    attacking <-
      case attack of
        Just target -> do
          hurt target 1
          whenM (hasAny @CObstacle [target]) (record ObstacleHurt)
          records [PlayerAttack]
          pure $ Just (CPlayer [PAttack])
        Nothing -> pure $ Just (CPlayer [])
    pure (attacking, moving, CStat Stat {life = life - 1})

finishAnimation :: forall c d. (Members World IO c, Get World IO c, Set World IO d) => d -> System' ()
finishAnimation to = cmap $ \(_ :: c, CAnimation time duration) ->
  eitherIf (const $ time >= duration) to

stepAnimation :: Double -> System' ()
stepAnimation dt = do
  cmap $ \(CLinear (Linear time from to)) -> Just (CLinear (Linear (time + dt) from to))
  cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)
  cmap $
    \(CPlayer ps, CAnimation time duration) -> case (time >= duration, ps) of
      (True, a : rest@(b : _)) -> Right (CPlayer rest, Player.animate b)
      _ -> Left ()

killObstacles :: System' ()
killObstacles = cmapM $ \(CObstacle _, CStat Stat {life}) ->
  if life > 0
    then pure $ Left ()
    else do
      record ObstacleDie
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
        hurt target 10
        modify target $ \(CPlayer state) -> CPlayer (PHurt : state)
        record PlayerHurt
        record EnemyAttack
        pure $ Right (state ZAttack VAttack)
      (_, True) -> pure $ Right (state ZIdle VIdle)
      _ -> pure $ Left (CPosition next, CLinear (Linear 0 pos next), state ZIdle VIdle)

stepItems :: Map.Map Position Entity -> System' ()
stepItems pickers = do
  pick @CFruit FruitPicked 20
  pick @CSoda SodaPicked 10
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
    killObstacles
    targets <- getEntities @CPlayer
    stepItems targets
    stepEnemies targets
    stepState
  win <- (\(CLatest latest) -> any isPlayerWin latest) <$> get global
  if not win
    then thisLevel
    else nextLevel
  where
    thisLevel :: System' World
    thisLevel = ask
    nextLevel :: System' World
    nextLevel = do
      (CLevel level) <- get global
      life <- cfold (\_ (_ :: CPlayer, CStat Stat {life}) -> life) 0
      lift $ do
        w <- initWorld
        runWith w $ do
          initialize (level + 1)
          cmap $ \(_ :: CPlayer) -> Just (CStat Stat {life = life + 1})
          ask
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

draw :: Env.Env -> SDL.Window -> SDL.Renderer -> System' ()
draw Env.Env {player, vampire, zombie, ground, music, enemy, wall, obstacle, prop, misc, font} w r = do
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
      drawObstacle obstacle,
      drawSheet prop,
      drawPlayer player,
      drawVampire vampire,
      drawZombie zombie
    ]
  (V2 w h) <- StateVar.get $ SDL.Video.windowSize w
  whenJust (Map.lookup 16 font) $ \f -> do
    food <- cfold (\_ (CPlayer _, CStat Stat {life}) -> life) 0
    let foodText = Text.pack ("Food " ++ show food)
    el <- mkTextElement r f foodText
    render r (V2 (w `div` 2) (h - 30)) el
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
    playerSound, miscSound, enemySound :: SDL.Mixer.Channel
    playerSound = 0
    miscSound = 1
    enemySound = 2
    playFootstep :: Env.Player -> System' ()
    playFootstep Env.Player {sfxFootstep} = playSound playerSound sfxFootstep
    playChop :: Env.Player -> System' ()
    playChop Env.Player {sfxChop} = playSound playerSound sfxChop
    playEnemyAttack :: Env.Enemy -> System' ()
    playEnemyAttack Env.Enemy {sfxAttack} = playSound enemySound [sfxAttack]
    playEnemyDie :: Env.Enemy -> System' ()
    playEnemyDie Env.Enemy {sfxDie} = playSound miscSound [sfxDie]
    playSoda :: Env.Misc -> System' ()
    playSoda Env.Misc {sfxSoda} = playSound miscSound sfxSoda
    playFruit :: Env.Misc -> System' ()
    playFruit Env.Misc {sfxFruit} = playSound miscSound sfxFruit
    toScreen :: Integral a => V2 Double -> V2 a
    toScreen p = floor . (* 32) <$> p
    interpolate :: Linear -> V2 Double
    interpolate (Linear t from to) = (fromIntegral <$> from) ^+^ (min (t / duration) 1 *^ (fromIntegral <$> (to ^-^ from)))
      where
        duration = 0.1
    drawToScreen :: forall s. Drawable s => (V2 Double, s) -> System' ()
    drawToScreen (p, a) = render r (toScreen p) a
    cdraw :: forall c s. (Drawable s, Members World IO c, Get World IO c) => (c -> Maybe (Position, s)) -> System' ()
    cdraw f = cmapM_ $ \c -> whenJust (f c) (\(pos, s) -> drawToScreen (fromIntegral <$> pos, s))
    drawSheet :: forall c. (Members World IO (Clip c), Get World IO (Clip c), Ord c) => Sheet c -> System' ()
    drawSheet s = cdraw $ \(Clip c, CPosition pos, _ :: Not CDead) ->
      Just (pos, (s {clip = Just c}))
    drawObstacle :: Map.Map Obstacle (Sheet ObstacleHealth) -> System' ()
    drawObstacle variants = cdraw $ \(CPosition pos, CObstacle variant, CStat Stat {life}, _ :: Not CDead) -> do
      sheet <- Map.lookup variant variants
      return (pos, sheet {clip = Just $ if life <= 1 then ODamaged else ONew})
    drawCreature :: forall c. (Members World IO c, Get World IO c) => (Linear -> Double -> Double -> c -> System' ()) -> System' ()
    drawCreature f = cmapM_ $ \(c :: c, _ :: Not CDead, CLinear lin, CAnimation time dur) -> f lin time dur c
    drawPlayer :: Env.Player -> System' ()
    drawPlayer Env.Player {idle, hurt, attack} = drawCreature @CPlayer $
      \lin time dur (CPlayer p) ->
        let go :: forall n. (KnownNat n, 1 <= n) => ASheet n -> System' ()
            go = drawToScreen . (interpolate lin,) . linear time dur
         in case listToMaybe p of
              (Just PIdle) -> go idle
              (Just PHurt) -> go hurt
              (Just PAttack) -> go attack
    drawVampire :: Env.Vampire -> System' ()
    drawVampire Env.Vampire {idle, attack} = drawCreature @CVampire $
      \lin time dur (CVampire p) ->
        let go :: forall n. (KnownNat n, 1 <= n) => ASheet n -> System' ()
            go = drawToScreen . (interpolate lin,) . linear time dur
         in case p of
              VIdle -> go idle
              VAttack -> go attack
    drawZombie :: Env.Zombie -> System' ()
    drawZombie Env.Zombie {idle, attack} = drawCreature @CZombie $
      \lin time dur (CZombie p) ->
        let go :: forall n. (KnownNat n, 1 <= n) => ASheet n -> System' ()
            go = drawToScreen . (interpolate lin,) . linear time dur
         in case p of
              ZIdle -> go idle
              ZAttack -> go attack

run :: World -> IO ()
run w = do
  w' <- runWith w (initialize 0 >> ask)
  play w' Env.resources events step draw
