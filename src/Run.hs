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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Run where

import Apecs
  ( Entity,
    Get,
    Has,
    Members,
    Not (..),
    Proxy (..),
    Set,
    ask,
    cfold,
    cfoldM,
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
import Control.Arrow
import Control.Monad (filterM, void, when, zipWithM_)
import qualified Control.Monad.State.Strict as State
import qualified Creature.Player as Player
import qualified Creature.Vampire as Vampire
import qualified Creature.Zombie as Zombie
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Draw (draw)
import Engine.SDL (play)
import qualified Env
import Event (Event (..), events)
import GHC.TypeNats
import Game.Component
import Game.World (All, System', World, initWorld)
import Helper.Extra (eitherIf, entitiesAt, getAny, hasAny, maybeIf, toTime, whenJust, whenM)
import Helper.Happened (isPlayerDie, isPlayerWin, isRestart)
import Linear ((*^), V2 (..), V4 (..), (^+^), (^-^))
import System.Random (RandomGen, mkStdGen, newStdGen, random, randomR, randomRs, randoms, setStdGen, split)

dirToV2 :: Direction -> Position
dirToV2 = \case
  East -> V2 distance 0
  West -> V2 (- distance) 0
  South -> V2 0 distance
  North -> V2 0 (- distance)
  where
    distance = 1

record :: Happened -> System' ()
record = modify global . (<>) . CLatest . (: [])

tick :: Double -> System' ()
tick dt = do
  modify global $ \(CTime time) -> CTime (time + toTime dt)
  (CTime time) <- get global
  set global (CLatest [])

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
      spawnArea = [V2 x y | x <- shrink 1 xs, y <- shrink 1 ys, V2 x y `notElem` [start, goal]]
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
    newEnumsAt :: (Set World IO (Clip a)) => [a] -> [Position] -> System' ()
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

hurt :: Entity -> Word -> System' ()
hurt e n =
  modify e $ \(CStat Stat {life}) -> CStat Stat {life = life - fromIntegral n}

recover :: Entity -> Word -> System' ()
recover e n =
  modify e $ \(CStat Stat {life}) -> CStat Stat {life = life + fromIntegral n}

stepPlayer :: Position -> System' ()
stepPlayer next =
  cmapM $ \(CPlayer state, CPosition pos, CStat Stat {life}) -> do
    record AteFood
    at <- entitiesAt next
    win <- hasAny @CGoal at
    attack <- getAny @(Not CPlayer, CStat) at
    occupied <- hasAny @(Not CGround, Not CSoda, Not CFruit) at
    when win $ record PlayerWin
    moving <-
      if not occupied
        then do
          record PlayerMove
          pure $ Right (CPosition next, CMove 0 pos next)
        else pure $ Left ()
    attacking <-
      case attack of
        Just target -> do
          hurt target 1
          record PlayerAttack
          pure $ Just (CPlayer [PAttack])
        Nothing -> pure $ Just (CPlayer [])
    pure (attacking, moving, CStat Stat {life = life - 1})

finishAnimation :: forall c d. (Members World IO c, Get World IO c, Set World IO d) => d -> System' ()
finishAnimation to = cmap $ \(_ :: c, CAnimation time duration) ->
  eitherIf (const $ time >= duration) to

stepAnimation :: Double -> System' ()
stepAnimation dt = do
  cmap $ \(CMove time from to) -> Just (CMove (time + dt) from to)
  cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)
  cmap $
    \(CPlayer ps, CAnimation time duration) -> case (time >= duration, ps) of
      (True, a : rest@(b : _)) -> Right (CPlayer rest, Player.animate b)
      _ -> Left ()

removeObstacles :: System' ()
removeObstacles = cmapM $ \(CObstacle _, CStat Stat {life}) ->
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
        let damage = 10
        hurt target damage
        modify target $ \(CPlayer state) -> CPlayer (PHurt : state)
        record (PlayerHurt damage)
        record EnemyAttack
        pure $ Right (state ZAttack VAttack)
      (_, True) -> pure $ Right (state ZIdle VIdle)
      _ -> pure $ Left (CPosition next, CMove 0 pos next, state ZIdle VIdle)

stepItems :: Map.Map Position Entity -> System' ()
stepItems pickers = do
  let fruit = 20
      soda = 20
  pick @CFruit (FruitPicked fruit) fruit
  pick @CSoda (SodaPicked soda) soda
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

stepLast :: System' ()
stepLast = do
  (CLatest latest) <- get global
  cmapM_ $ \(_ :: CPlayer, CStat Stat {life}) -> when (life <= 0) (record PlayerDie)
  cmap $ \(CPlayer state) -> let state' = reverse (PIdle : state) in Just (CPlayer state', Player.animate (head state'))
  cmap $ \(CZombie state) -> Just (Zombie.animate state)
  cmap $ \(CVampire state) -> Just (Vampire.animate state)

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

step :: Env.Env -> Double -> [Event] -> System' ()
step env dt events = do
  removeDead
  tick dt
  stepAnimation dt
  game <- get global
  case game of
    GamePlay -> do
      shouldUpdate <- evalNext events
      whenJust shouldUpdate $ \next -> do
        stepPlayer next
        removeObstacles
        targets <- getEntities @CPlayer
        stepItems targets
        stepEnemies targets
        stepLast
    LevelStart -> when (EnterPressed `elem` events) $ set global GamePlay
    GameOver -> when (EnterPressed `elem` events) $ record Restart
  where
    getEntities :: forall c. (Members World IO c, Get World IO c) => System' (Map.Map Position Entity)
    getEntities = cfold (\acc (_ :: c, e, CPosition pos) -> Map.insert pos e acc) mempty

change :: Env.Env -> System' World
change _ = do
  (CLatest latest) <- get global
  let win = any isPlayerWin latest
      restart = any isRestart latest
      dead = any isPlayerDie latest
  when dead $ set global GameOver
  case (win, restart) of
    (_, True) -> zeroLevel
    (True, _) -> nextLevel
    _ -> thisLevel
  where
    thisLevel :: System' World
    thisLevel = ask
    zeroLevel :: System' World
    zeroLevel =
      lift $ do
        w <- initWorld
        runWith w $ do
          initialize 0
          ask
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

run :: World -> IO ()
run w = do
  w' <- runWith w (initialize 0 >> ask)
  play w' Env.resources events step draw change
