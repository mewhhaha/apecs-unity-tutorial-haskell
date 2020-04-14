{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Env where

import Apecs.SDL.Internal (ASheet, Sheet, Texture, loadTexture, mkASheet, mkClips, mkRect, mkSheet)
import qualified Data.Map as Map
import Foreign.C.Types
import GHC.Natural
import GHC.TypeNats
import qualified Game.Component as C
import qualified SDL
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

data Misc = Misc {sfxSoda :: [SDL.Mixer.Chunk], sfxFruit :: [SDL.Mixer.Chunk], sfxEnemy :: [SDL.Mixer.Chunk]}

data Env
  = Env
      { misc :: Misc,
        zombie :: Zombie,
        vampire :: Vampire,
        player :: Player,
        prop :: Sheet C.Prop,
        ground :: Sheet C.Ground,
        obstacle :: Sheet C.Obstacle,
        wall :: Sheet C.Wall
      }

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
  sfxFootstep <- loadAudios ["scavengers_footstep1.aif", "scavengers_footstep2.aif"]
  sfxSoda <- loadAudios ["scavengers_soda1.ogg", "scavengers_soda2.ogg"]
  sfxFruit <- loadAudios ["scavengers_fruit1.aif", "scavengers_fruit2.aif"]
  sfxChop <- loadAudios ["scavengers_chop1.ogg", "scavengers_chop2.ogg"]
  sfxEnemy <- loadAudios ["scavengers_enemy1.aif", "scavengers_enemy2.aif"]
  sfxMusic <- loadAudio "scavengers_music.ogg"
  sfxDie <- loadAudio "scavengers_die.aif"
  SDL.Mixer.setMusicVolume 20
  SDL.Mixer.playMusic SDL.Mixer.Forever sfxMusic
  pure
    Env.Env
      { prop = prop,
        ground = ground,
        obstacle = obstacle,
        wall = wall,
        misc =
          Env.Misc
            { sfxEnemy = sfxEnemy,
              sfxSoda = sfxSoda,
              sfxFruit = sfxFruit
            },
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
    loadAudios :: SDL.Mixer.Loadable a => [FilePath] -> IO [a]
    loadAudios = traverse loadAudio
    loadAudio :: SDL.Mixer.Loadable a => FilePath -> IO a
    loadAudio f = SDL.Mixer.load ("resources" </> "audio" </> f)
    loadImage :: FilePath -> IO Texture
    loadImage f = loadTexture r ("resources" </> "sprites" </> f)
    loadSheet32x32 :: forall a. (Enum a, Ord a, Bounded a) => FilePath -> IO (Sheet a)
    loadSheet32x32 f = (`mkSheet` mkClips (32, 32)) <$> loadImage f
    loadASheet32x32 :: forall n. (KnownNat n, 1 <= n) => FilePath -> IO (ASheet n)
    loadASheet32x32 f = (`mkASheet` (32, 32)) <$> loadImage f
