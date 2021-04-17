module Game.Level where

import Apecs qualified
import Data.Set (elemAt, member)
import Relude hiding (state)
import Relude.Extra (size)
import Relude.Monad.Reexport qualified as State
import SDL qualified
import System.Random (Random (randoms), RandomGen, mkStdGen, newStdGen, randomR, randomRs, setStdGen, split)
import World (System', World)
import World.Component

edges :: Set Position
edges = fromList $ uncurry SDL.V2 <$> (xs (,0) ++ xs (,14) ++ ys (0,) ++ ys (19,))
  where
    xs f = f <$> [0 .. 20]
    ys f = f <$> [0 .. 14]

shuffle :: RandomGen r => r -> [a] -> [a]
shuffle r = fmap snd . sortBy (compare `on` fst) . zip (randoms @Int r)

pick :: RandomGen r => r -> Int -> State.State [a] [a]
pick r n = do
  state <- State.get
  let (picked, rest) = splitAt n (shuffle r state)
  State.put rest
  return picked

spread :: RandomGen r => Int -> r -> State.State [Position] [Position]
spread origins originalGen = do
  positions <- State.get
  return (toList . fromList @(Set Position) . concat $ origin positions <$> gs)
  where
    decreaseChance, initialChance :: Double
    decreaseChance = 0.1
    initialChance = 0.4
    gs = take origins $ iterate (snd . split) originalGen
    origin :: RandomGen r => [Position] -> r -> [Position]
    origin positions originGen = expand g'' initialChance n'
      where
        ps = fromList positions
        (n', g'') = randPos originGen ps
        randPos :: RandomGen r => r -> Set Position -> (Position, r)
        randPos g xs = let (n, g') = randomR (0, size xs - 1) g in (elemAt n xs, g')
        expand :: RandomGen r => r -> Double -> Position -> [Position]
        expand g chance n = n : concat (expand (snd . split $ g) (chance - decreaseChance) <$> neighbours)
          where
            neighbours =
              catMaybes
                . zipWith (\c p -> if c < chance then Just p else Nothing) (randomRs (0.0, 1.0) g)
                . filter (`member` ps)
                $ (n +) <$> [SDL.V2 0 (-1), SDL.V2 0 1, SDL.V2 1 0, SDL.V2 (-1) 0]

newPlayer :: MonadIO m => Position -> Int -> Apecs.SystemT World m ()
newPlayer position life =
  Apecs.newEntity
    ( CPosition position,
      CPlayer,
      CAnimation (Animation 0 PlayerIdle 7),
      CLife life,
      CLerpPosition (LerpPosition 0 position position)
    )
    >> pass

newZombies :: MonadIO m => [Position] -> Apecs.SystemT World m ()
newZombies = mapM_ new
  where
    new position =
      Apecs.newEntity
        ( CPosition position,
          CEnemy Zombie,
          CAnimation (Animation 0 ZombieIdle 7),
          CLife 1,
          CLerpPosition (LerpPosition 0 position position)
        )

newVampires :: MonadIO m => [Position] -> Apecs.SystemT World m ()
newVampires = mapM_ new
  where
    new position =
      Apecs.newEntity
        ( CPosition position,
          CEnemy Vampire,
          CAnimation (Animation 0 VampireIdle 7),
          CLife 1,
          CLerpPosition (LerpPosition 0 position position)
        )

newObstacles :: MonadIO m => [Position] -> Apecs.SystemT World m ()
newObstacles = mapM_ new
  where
    new position = Apecs.newEntity (CPosition position, CEnemy Obstacle, CLife 2)

newSodas :: MonadIO m => [Position] -> Apecs.SystemT World m ()
newSodas = mapM_ new
  where
    new position = Apecs.newEntity (CPosition position, CFood Soda)

newFruit :: MonadIO m => [Position] -> Apecs.SystemT World m ()
newFruit = mapM_ new
  where
    new position = Apecs.newEntity (CPosition position, CFood Fruit)

newLevel :: MonadIO m => Int -> Apecs.SystemT World m ()
newLevel l = Apecs.set Apecs.global (CLevel (Level l))

destroyComponents :: Apecs.Not (CPlayer, CEnemy, CAnimation, CPosition, CFood, CLife, CDead, CLerpPosition)
destroyComponents = Apecs.Not

cleanLevel :: System' ()
cleanLevel = Apecs.cmap $ \(_ :: CPosition) -> destroyComponents

row :: NonEmpty Int
row = 1 :| [2 .. 18]

column :: NonEmpty Int
column = 1 :| [2 .. 13]

start :: SDL.V2 Int
start = SDL.V2 1 (last column)

goal :: SDL.V2 Int
goal = SDL.V2 (last row) 1

createLevel :: MonadIO m => Int -> Int -> Apecs.SystemT World m ()
createLevel l life = do
  liftIO $ setStdGen (mkStdGen l)
  g <- liftIO newStdGen
  let area = [SDL.V2 x y | x <- toList row, y <- toList column, SDL.V2 x y `notElem` [start, goal]]
      [zombies, vampires, sodas, fruit, obstacles] =
        flip State.evalState area $
          sequence [pick g 5, pick g 3, pick g 3, pick g 3, spread 10 g]

  newPlayer (SDL.V2 1 13) life
  newZombies zombies
  newVampires vampires
  newSodas sodas
  newFruit fruit
  newObstacles obstacles
  newLevel l

createLevelOne :: System' ()
createLevelOne = createLevel 1 100