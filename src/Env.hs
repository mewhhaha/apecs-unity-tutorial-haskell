{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Env
  ( resources,
    Zombie (..),
    Vampire (..),
    Player (..),
    Misc (..),
    Env (..),
    Enemy (..),
  )
where

import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Engine.SDL.Internal (ASheet, Sheet, Texture, loadTexture, mkASheet, mkClips, mkRect, mkSheet)
import GHC.TypeNats
import qualified Game.Component as C
import qualified SDL
import qualified SDL.Font
import qualified SDL.Mixer
import System.FilePath ((</>))

data Zombie
  = Zombie
      { idle :: ASheet 6,
        attack :: ASheet 2
      }

data Vampire
  = Vampire
      { idle :: ASheet 6,
        attack :: ASheet 2
      }

data Player
  = Player
      { idle :: ASheet 6,
        attack :: ASheet 2,
        hurt :: ASheet 2,
        sfxFootstep :: [SDL.Mixer.Chunk],
        sfxChop :: [SDL.Mixer.Chunk],
        sfxDie :: SDL.Mixer.Chunk
      }

data Misc = Misc {sfxSoda :: [SDL.Mixer.Chunk], sfxFruit :: [SDL.Mixer.Chunk]}

newtype Enemy = Enemy {sfxAttack :: [SDL.Mixer.Chunk]}

data Env
  = Env
      { misc :: Misc,
        font :: Map.Map SDL.Font.PointSize SDL.Font.Font,
        zombie :: Zombie,
        music :: ByteString.ByteString,
        enemy :: Enemy,
        vampire :: Vampire,
        player :: Player,
        prop :: Sheet C.Prop,
        ground :: Sheet C.Ground,
        obstacle :: Map.Map C.Obstacle (Sheet C.ObstacleHealth),
        wall :: Sheet C.Wall
      }

times32 :: Integral i => i -> i
times32 x = x * 32

xy32 :: Integral i => i -> i -> (i, i)
xy32 x y = (times32 x, times32 y)

obstacles :: Texture -> Map.Map C.Obstacle (Sheet C.ObstacleHealth)
obstacles sheet =
  Map.fromList $
    fmap (mkSheet sheet)
      <$> [ (C.O1, make (xy32 5 2) (xy32 0 6)),
            (C.O2, make (xy32 6 2) (xy32 1 6)),
            (C.O3, make (xy32 7 2) (xy32 2 6)),
            (C.O4, make (xy32 0 3) (xy32 3 6)),
            (C.O5, make (xy32 3 3) (xy32 4 6)),
            (C.O6, make (xy32 5 3) (xy32 4 6)),
            (C.O7, make (xy32 6 3) (xy32 5 6)),
            (C.O8, make (xy32 7 3) (xy32 6 6))
          ]
  where
    mkRect32 (x, y) = mkRect x y (times32 1) (times32 1)
    make new damaged = Map.fromList [(C.ONew, mkRect32 new), (C.ODamaged, mkRect32 damaged)]

resources :: SDL.Renderer -> IO Env.Env
resources r = do
  loadedFont <- loadFont "PressStart2P-Regular.ttf"
  sheet <- loadImage "Scavengers_SpriteSheet.png"
  let loadedProp = loadSheet32x32 (xy32 2 2) sheet
      loadedGround = loadSheet32x32 (xy32 0 4) sheet
      loadedWall = mkSheet sheet (Map.fromList [(C.W1, mkRect32 (xy32 1 3)), (C.W2, mkRect32 (xy32 2 3)), (C.W3, mkRect32 (xy32 4 3))])
      loadedObstacle = obstacles sheet
      playerAttack = loadASheet32x32 (xy32 0 5) sheet
      playerIdle = loadASheet32x32 (xy32 0 0) sheet
      playerHurt = loadASheet32x32 (xy32 4 5) sheet
      vampireIdle = loadASheet32x32 (xy32 4 1) sheet
      vampireAttack = loadASheet32x32 (xy32 4 5) sheet
      zombieIdle = loadASheet32x32 (xy32 6 0) sheet
      zombieAttack = loadASheet32x32 (xy32 2 5) sheet
  loadedFootstep <- loadAudios ["scavengers_footstep1.aif", "scavengers_footstep2.aif"]
  loadedSoda <- loadAudios ["scavengers_soda1.ogg", "scavengers_soda2.ogg"]
  loadedFruit <- loadAudios ["scavengers_fruit1.aif", "scavengers_fruit2.aif"]
  loadedChop <- loadAudios ["scavengers_chop1.ogg", "scavengers_chop2.ogg"]
  loadedEnemyAttack <- loadAudios ["scavengers_enemy1.aif", "scavengers_enemy2.aif"]
  loadedMusic <- ByteString.readFile ("resources" </> "audio" </> "scavengers_music.ogg")
  loadedDie <- loadAudio "scavengers_die.aif"
  pure
    Env.Env
      { prop = loadedProp,
        font = loadedFont,
        music = loadedMusic,
        ground = loadedGround,
        obstacle = loadedObstacle,
        wall = loadedWall,
        misc =
          Env.Misc
            { sfxSoda = loadedSoda,
              sfxFruit = loadedFruit
            },
        enemy = Env.Enemy {sfxAttack = loadedEnemyAttack},
        player =
          Env.Player
            { attack = playerAttack,
              idle = playerIdle,
              hurt = playerHurt,
              sfxFootstep = loadedFootstep,
              sfxChop = loadedChop,
              sfxDie = loadedDie
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
    mkRect32 (x, y) = mkRect x y (times32 1) (times32 1)
    loadAudios :: SDL.Mixer.Loadable a => [FilePath] -> IO [a]
    loadAudios = traverse loadAudio
    loadAudio :: SDL.Mixer.Loadable a => FilePath -> IO a
    loadAudio f = SDL.Mixer.load ("resources" </> "audio" </> f)
    loadFont :: FilePath -> IO (Map.Map SDL.Font.PointSize SDL.Font.Font)
    loadFont f = do
      let sizes = [16, 18]
      fonts <- mapM (SDL.Font.load ("resources" </> "fonts" </> f)) sizes
      return . Map.fromList $ zip sizes fonts
    loadImage :: FilePath -> IO Texture
    loadImage f = loadTexture r ("resources" </> "sprites" </> f)
    loadSheet32x32 :: forall a. (Enum a, Ord a, Bounded a) => (Int, Int) -> Texture -> Sheet a
    loadSheet32x32 origin = mkSheet <$> id <*> mkClips (xy32 1 1) origin
    loadASheet32x32 :: forall n. (KnownNat n, 1 <= n) => (Int, Int) -> Texture -> ASheet n
    loadASheet32x32 = mkASheet (xy32 1 1)
