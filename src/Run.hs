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
import qualified Control.Monad.State.Strict as State
import qualified Creature.Player as Player
import qualified Creature.Vampire as Vampire
import qualified Creature.Zombie as Zombie
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
import Game.World (System', World)
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

initialize :: System' ()
initialize = do
  lift $ setStdGen (mkStdGen 1)
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
    [] -> Left ()
    [PAttack] -> toComp PAttack [PIdle]
    (PHurt : _) -> toComp PHurt []
    (a : as) -> toComp a as
  where
    toComp a as = Right (CPlayer (a :| as), Player.animate a)
    toState (Hurt _) = Just PHurt
    toState Attack = Just PAttack
    toState (Movement _) = Just PIdle

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
  pick @CSoda 1
  where
    pick :: forall c. (Members World IO c, Get World IO c) => Word -> System' ()
    pick i = cmapM_ $ \(_ :: c, CPosition pos, self :: Entity) ->
      whenJust (Map.lookup pos pickers) $ \picker -> do
        send picker (Recover i)
        send self Die

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
    evalItems player
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

isAttack :: Action -> Bool
isAttack Attack = True
isAttack _ = False

draw :: Env.Env -> SDL.Renderer -> System' ()
draw Env.Env {player, vampire, zombie, ground, wall, obstacle, prop} r = do
  cmapM_ $ \(_ :: CPlayer, CActionStream stream) -> do
    let latest = snd . NonEmpty.head $ stream
    when (any isMovement latest) (playFootstep player)
    when (any isAttack latest) (playChop player)
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
    playPlayerSound :: [SDL.Mixer.Chunk] -> System' ()
    playPlayerSound chunks = do
      g <- lift newStdGen
      let randomIndex = fst (randomR (0, length chunks - 1) g)
      SDL.Mixer.setVolume 20 (0 :: SDL.Mixer.Channel)
      void $ SDL.Mixer.playOn 0 SDL.Mixer.Once (chunks !! randomIndex)
    playFootstep :: Env.Player -> System' ()
    playFootstep Env.Player {sfxFootstep} = playPlayerSound sfxFootstep
    playChop :: Env.Player -> System' ()
    playChop Env.Player {sfxChop} = playPlayerSound sfxChop
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
  w' <- runWith w (initialize >> ask)
  play w' Env.resources events step draw
