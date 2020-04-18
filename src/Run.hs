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
import Control.Monad (forM_, unless, void, when)
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

send :: Entity -> Action -> System' ()
send e action = modify e $ \(CActionStream ((t, as) :| rest)) -> CActionStream ((t, action : as) :| rest)

sends :: Entity -> [Action] -> System' ()
sends e actions = modify e $ \(CActionStream ((t, as) :| rest)) -> CActionStream ((t, actions ++ as) :| rest)

tick :: Double -> System' ()
tick dt = do
  modify global $ \(CTime time) -> CTime (time + toTime dt)
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

spread :: RandomGen r => Int -> r -> State.State [V2 Double] [V2 Double]
spread starts g = do
  positions <- State.get
  return (Set.toList . Set.fromList . concat $ start positions <$> gs)
  where
    decreaseChance = 0.1
    initialChance = 0.4
    gs = take starts $ iterate (snd . split) g
    start :: RandomGen r => [V2 Double] -> r -> [V2 Double]
    start positions g = expand g' initialChance n
      where
        ps = Set.fromList positions
        (n, g') = randPos g ps
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
              new c = void $ newEntity (c, CPosition pos, Clip p, CActionStream ((0, []) :| []))
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
    xs :: [Double]
    xs = [0 .. 19]
    ys :: [Double]
    ys = [0 .. 14]

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
playerAttack enemies next = cmapM_ $ \(_ :: CPlayer, CPosition pos, player :: Entity) ->
  whenJust (Map.lookup next enemies) $ \enemy -> do
    send enemy (Hurt 1)
    send player Attack

playerMove :: Set.Set Position -> Position -> System' ()
playerMove occupied next = cmapM_ $ \(CPlayer _, CPosition pos, player :: Entity) ->
  when (Set.notMember next occupied) $ send player (Movement next)

finishAnimation :: forall c d. (Members World IO c, Get World IO c, Set World IO d) => d -> System' ()
finishAnimation to = cmap $ \(_ :: c, CAnimation time duration) ->
  eitherIf (const $ time >= duration) to

stepAnimation :: Double -> System' ()
stepAnimation dt = do
  cmap $ \(CAnimation time duration) -> Just (CAnimation (time + dt) duration)
  cmap $
    \(CPlayer ps, CAnimation time duration) -> case (time >= duration, snd (NonEmpty.uncons ps)) of
      (True, Just rest) -> Right (CPlayer rest, Player.animate . NonEmpty.head $ rest)
      _ -> Left ()

killEnemies :: System' ()
killEnemies = cmapM $ \(CEnemy, CStat Stat {hitpoints}, enemy :: Entity) ->
  pure $ eitherIf (const $ hitpoints <= 0) CDead

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

evalEvents :: [Event] -> Map.Map Position Entity -> Set.Set Position -> Set.Set Position -> System' ()
evalEvents events enemies goal occupied =
  cmapM_ $ \(CPlayer _, CPosition prev, player :: Entity) ->
    whenJust (maybeIf (/= prev) $ move events prev) $ \next ->
      case (Set.member next occupied, Map.lookup next enemies, Set.member next goal) of
        (_, _, True) -> void $ newEntity CWin
        (_, Just enemy, _) -> do
          send enemy (Hurt 1)
          send player Attack
        (True, _, _) -> pure ()
        _ -> send player (Movement next)
  where
    move :: [Event] -> Position -> Position
    move = (+) . sum . mapMaybe dir
      where
        dir (InputMove d) = Just (dirToV2 d)
        dir _ = Nothing

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
statePlayer = peekLatest @CPlayer $ \latest ->
  case Set.elems . Set.fromList $ mapMaybe toState latest of
    [PAttack] -> toComp PAttack [PIdle]
    (PHurt : _) -> toComp PHurt []
    (a : as) -> toComp a as
    _ -> Left ()
  where
    toComp a as = Right (CPlayer (a :| as), Player.animate a)
    toState (Hurt _) = Just PHurt
    toState Attack = Just PAttack
    toState _ = Just PIdle

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

evalItems :: Map.Map Position Entity -> System' ()
evalItems pickers = do
  pick @CFruit 1
  pick @CSoda 2
  where
    pick :: forall c. (Members World IO c, Get World IO c) => Word -> System' ()
    pick i = cmapM $ \(_ :: c, CPosition pos, self :: Entity) ->
      case Map.lookup pos pickers of
        Nothing -> pure $ Left ()
        Just picker -> do
          send picker (Recover i)
          pure $ Right CDead

removeDead :: System' ()
removeDead = cmap $ \CDead -> Not @All

step :: Env.Env -> Double -> [Event] -> System' World
step _ dt actions = do
  removeDead
  tick dt
  stepAnimation dt
  let shouldUpdate = not . null $ actions
  when shouldUpdate $ do
    walls <- getEntities @CWall
    obstacles <- getEntities @CObstacle
    enemies <- getEntities @CEnemy
    goal <- Map.keysSet <$> getEntities @CGoal
    let occupied = foldl1 Set.union $ Map.keysSet <$> [walls, obstacles, enemies]
    evalEvents actions enemies goal occupied
    hurtEnemies
    killEnemies
    movePlayer
    player <- getEntities @CPlayer
    evalItems player
    evalEnemies player occupied
    hurtPlayer
    moveEnemies
    statePlayer
    stateZombie
    stateVampire
  win <- cfold (\_ CWin -> True) False
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

isMovement :: Action -> Bool
isMovement (Movement _) = True
isMovement _ = False

isAttack :: Action -> Bool
isAttack Attack = True
isAttack _ = False

whenHas :: forall c. (Members World IO c, Get World IO c) => System' () -> System' ()
whenHas op = do
  has <- cfold (\prev (_ :: c) -> prev || True) False
  when has op

draw :: Env.Env -> SDL.Renderer -> System' ()
draw Env.Env {player, vampire, zombie, ground, music, enemy, wall, obstacle, prop, misc} r = do
  isMusicPlay <- SDL.Mixer.playingMusic
  unless isMusicPlay (playMusic music)
  cmapM_ $ \(_ :: CPlayer, CActionStream stream) -> do
    let latest = snd . NonEmpty.head $ stream
    when (any isMovement latest) (playFootstep player)
    when (any isAttack latest) (playChop player)
  whenHas @(CSoda, CDead) (playSoda misc)
  whenHas @(CFruit, CDead) (playFruit misc)
  whenHas @(CEnemy, CDead) (playEnemyDie enemy)
  enemyAttack <-
    cfold
      ( \prev (CEnemy, CActionStream stream) ->
          let latest = snd . NonEmpty.head $ stream in prev || (any isAttack latest)
      )
      False
  when enemyAttack (playEnemyAttack enemy)
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
    toScreen :: Integral a => V2 Double -> V2 a
    toScreen p = round . (* 32) <$> p
    render :: forall s. (Sprite s) => (V2 Double, s) -> IO ()
    render (p, a) = renderSprite r (toScreen p) a
    cdraw :: forall c s. (Sprite s, Members World IO c, Get World IO c) => (c -> (V2 Double, s)) -> System' ()
    cdraw f = cfoldM (\acc c -> pure (acc >> render (f c))) mempty >>= lift
    drawSheet :: forall c. (Members World IO (Clip c), Get World IO (Clip c), Ord c) => Sheet c -> System' ()
    drawSheet s = cdraw $ \(Clip c, CPosition pos, _ :: Not CDead) ->
      (pos, (s {clip = Just c}))
    drawAnimation :: forall c. (Members World IO c, Get World IO c) => (V2 Double -> Double -> Double -> c -> IO ()) -> System' ()
    drawAnimation f = cfoldM (\acc (c :: c, CPosition pos, _ :: Not CDead, CAnimation time dur) -> pure (acc >> f pos time dur c)) mempty >>= lift
    drawPlayer :: Env.Player -> System' ()
    drawPlayer Env.Player {idle, hurt, attack} = drawAnimation @CPlayer $
      \pos time dur (CPlayer p) ->
        let render' :: forall n. (KnownNat n, 1 <= n) => ASheet n -> IO ()
            render' = render . (pos,) . linear time dur
         in case NonEmpty.head p of
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
  w' <- runWith w (initialize 0 >> ask)
  play w' Env.resources events step draw
