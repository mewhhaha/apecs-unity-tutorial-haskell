{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Env where

import Control.Monad
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map
import Engine.SDL.Internal (ASheet, Sheet, Texture, loadTexture, mkASheet, mkClips, mkRect, mkSheet)
import Foreign.C.Types
import GHC.Natural
import GHC.TypeNats
import qualified Game.Component as C
import Linear
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

obstacles :: Texture -> Map.Map C.Obstacle (Sheet C.ObstacleHealth)
obstacles sheet =
  Map.fromList $
    fmap (mkSheet sheet)
      <$> [ (C.O1, obstacle (x32 5, x32 2) (0, x32 6)),
            (C.O2, obstacle (x32 6, x32 2) (x32 1, x32 6)),
            (C.O3, obstacle (x32 7, x32 2) (x32 2, x32 6)),
            (C.O4, obstacle (x32 0, x32 3) (x32 3, x32 6)),
            (C.O5, obstacle (x32 3, x32 3) (x32 4, x32 6)),
            (C.O6, obstacle (x32 5, x32 3) (x32 4, x32 6)),
            (C.O7, obstacle (x32 6, x32 3) (x32 5, x32 6)),
            (C.O8, obstacle (x32 7, x32 3) (x32 6, x32 6))
          ]
  where
    x32 x = x * 32
    mkRect32 (x, y) = mkRect x y 32 32
    obstacle new damaged = Map.fromList [(C.ONew, mkRect32 new), (C.ODamaged, mkRect32 damaged)]

resources :: SDL.Renderer -> IO Env.Env
resources r = do
  font <- loadFont "PressStart2P-regular.ttf"
  sheet <- loadImage "Scavengers_SpriteSheet.png"
  let prop = loadSheet32x32 (64, 64) sheet
      ground = loadSheet32x32 (0, 128) sheet
      wall = mkSheet sheet (Map.fromList [(C.W1, mkRect32 32 96), (C.W2, mkRect32 64 96), (C.W3, mkRect32 128 96)])
      obstacle = obstacles sheet
      playerAttack = loadASheet32x32 (0, 160) sheet
      playerIdle = loadASheet32x32 (0, 0) sheet
      playerHurt = loadASheet32x32 (192, 160) sheet
      vampireIdle = loadASheet32x32 (128, 32) sheet
      vampireAttack = loadASheet32x32 (128, 160) sheet
      zombieIdle = loadASheet32x32 (192, 0) sheet
      zombieAttack = loadASheet32x32 (64, 160) sheet
  sfxFootstep <- loadAudios ["scavengers_footstep1.aif", "scavengers_footstep2.aif"]
  sfxSoda <- loadAudios ["scavengers_soda1.ogg", "scavengers_soda2.ogg"]
  sfxFruit <- loadAudios ["scavengers_fruit1.aif", "scavengers_fruit2.aif"]
  sfxChop <- loadAudios ["scavengers_chop1.ogg", "scavengers_chop2.ogg"]
  sfxEnemyAttack <- loadAudios ["scavengers_enemy1.aif", "scavengers_enemy2.aif"]
  sfxMusic <- ByteString.readFile ("resources" </> "audio" </> "scavengers_music.ogg")
  sfxDie <- loadAudio "scavengers_die.aif"
  pure
    Env.Env
      { prop = prop,
        font = font,
        music = sfxMusic,
        ground = ground,
        obstacle = obstacle,
        wall = wall,
        misc =
          Env.Misc
            { sfxSoda = sfxSoda,
              sfxFruit = sfxFruit
            },
        enemy = Env.Enemy {sfxAttack = sfxEnemyAttack},
        player =
          Env.Player
            { attack = playerAttack,
              idle = playerIdle,
              hurt = playerHurt,
              sfxFootstep = sfxFootstep,
              sfxChop = sfxChop,
              sfxDie = sfxDie
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
    mkRect32 x y = mkRect x y 32 32
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
    loadSheet32x32 origin = mkSheet <$> id <*> mkClips (32, 32) origin
    loadASheet32x32 :: forall n. (KnownNat n, 1 <= n) => (Int, Int) -> Texture -> ASheet n
    loadASheet32x32 = mkASheet (32, 32)
