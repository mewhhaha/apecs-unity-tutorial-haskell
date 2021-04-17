{-# LANGUAGE RecordWildCards #-}

module Game.System.Draw (system) where

import Apecs qualified
import Game.Level (edges)
import Game.Resource (resourceSound, resourceSprite)
import Optics.Core (over, view)
import Relude
import Relude.Unsafe ((!!))
import SDL qualified
import SDL.Draw (drawSprite)
import SDL.Mixer qualified as SDLMixer
import System.FilePath ((</>))
import System.Random (Random (random, randomRIO, randoms), mkStdGen)
import Types (Tick (readDeltaTime, readRenderer))
import World (System', tickState)
import World.Component

scavengerSpritesheet :: Integral b => (b, b) -> b -> NonEmpty (SDL.Rectangle b)
scavengerSpritesheet (x, y) frames = go 0 :| (go <$> [1 .. frames - 1])
  where
    go i = SDL.Rectangle (SDL.P (SDL.V2 ox oy) * 32) 32
      where
        ox = (x + i) `mod` 8
        oy = y + ((x + i) `div` 8)

edgeSprites :: NonEmpty (SDL.Rectangle Integer)
edgeSprites = scavengerSpritesheet (1, 3) 4

obstacleSprites :: NonEmpty (SDL.Rectangle Integer)
obstacleSprites =
  scavengerSpritesheet (5, 2) 4 <> scavengerSpritesheet (5, 3) 3

brokenObstacleSprites :: NonEmpty (SDL.Rectangle Integer)
brokenObstacleSprites = scavengerSpritesheet (0, 6) 7

groundSprites :: NonEmpty (SDL.Rectangle Integer)
groundSprites = scavengerSpritesheet (0, 4) 8

playerIdleSprites :: NonEmpty (SDL.Rectangle Integer)
playerIdleSprites = scavengerSpritesheet (0, 0) 6

playerAttackSprites :: NonEmpty (SDL.Rectangle Integer)
playerAttackSprites = scavengerSpritesheet (0, 5) 2

playerHurtSprites :: NonEmpty (SDL.Rectangle Integer)
playerHurtSprites = scavengerSpritesheet (5, 5) 2

zombieIdleSprites :: NonEmpty (SDL.Rectangle Integer)
zombieIdleSprites = scavengerSpritesheet (6, 0) 6

zombieAttackSprites :: NonEmpty (SDL.Rectangle Integer)
zombieAttackSprites = scavengerSpritesheet (2, 5) 2

vampireIdleSprites :: NonEmpty (SDL.Rectangle Integer)
vampireIdleSprites = scavengerSpritesheet (4, 1) 6

vampireAttackSprites :: NonEmpty (SDL.Rectangle Integer)
vampireAttackSprites = scavengerSpritesheet (4, 5) 2

pickupSprites :: NonEmpty (SDL.Rectangle Integer)
pickupSprites = scavengerSpritesheet (2, 2) 2

goalSprite :: SDL.Rectangle Integer
goalSprite = head $ scavengerSpritesheet (4, 2) 1

toRect :: SDL.V2 Int -> SDL.Rectangle Int
toRect position = SDL.Rectangle (SDL.P position * 32) 32

ground :: Set Position
ground = fromList [SDL.V2 x y | x <- [1 .. 18], y <- [1 .. 13]]

playerAttackAnimation :: Animation
playerAttackAnimation = Animation 0 PlayerAttack 7

zombieAttackAnimation :: Animation
zombieAttackAnimation = Animation 0 ZombieAttack 7

vampireAttackAnimation :: Animation
vampireAttackAnimation = Animation 0 VampireAttack 7

playerIdleAnimation :: Animation
playerIdleAnimation = Animation 0 PlayerIdle 7

zombieIdleAnimation :: Animation
zombieIdleAnimation = Animation 0 ZombieIdle 7

vampireIdleAnimation :: Animation
vampireIdleAnimation = Animation 0 VampireIdle 7

playerHurtAnimation :: Animation
playerHurtAnimation = Animation 0 PlayerHurt 7

playRandomChunk :: SDLMixer.Channel -> [SDLMixer.Chunk] -> System' ()
playRandomChunk channel chunks = do
  randomIndex <- liftIO $ randomRIO (0, length chunks - 1)
  let chunk = chunks !! randomIndex
  SDLMixer.setVolume 20 chunk
  _ <- SDLMixer.playOn channel 1 chunk
  pass

musicChannel :: SDLMixer.Channel
musicChannel = 3

system :: System' ()
system = do
  (CScene scene) <- Apecs.get Apecs.global
  case scene of
    MainMenu _ _ -> pass
    GameOver _ -> pass
    LevelTitle _ -> pass
    Game _ -> do
      (CLevel (Level l), CHappenings happenings) <- Apecs.get Apecs.global
      let g = mkStdGen l
      dt <- tickState readDeltaTime
      renderer <- tickState readRenderer
      spritesheet <- resourceSprite renderer ("sprites" </> "Scavengers_SpriteSheet.png")
      chunksChop <- mapM (resourceSound . ("audio" </>)) ["scavengers_chop1.ogg", "scavengers_chop2.ogg"]
      chunksFootstep <- mapM (resourceSound . ("audio" </>)) ["scavengers_footstep1.aif", "scavengers_footstep2.aif"]
      chunksFruit <- mapM (resourceSound . ("audio" </>)) ["scavengers_fruit1.aif", "scavengers_fruit2.aif"]
      chunksEnemy <- mapM (resourceSound . ("audio" </>)) ["scavengers_enemy1.aif", "scavengers_enemy2.aif"]
      chunksSoda <- mapM (resourceSound . ("audio" </>)) ["scavengers_soda1.ogg", "scavengers_soda2.ogg"]
      chunkMusic <- resourceSound ("audio" </> "scavengers_music.ogg")

      playingMusic <- SDLMixer.playing musicChannel
      pausedMusic <- SDLMixer.paused musicChannel
      when (not playingMusic || pausedMusic) $ do
        SDLMixer.setVolume 20 chunkMusic
        _ <- SDLMixer.playOn musicChannel 2 chunkMusic
        pass

      Apecs.cmap $ \(CAnimation animation) -> CAnimation (over timer (+ dt * view speed animation) animation)
      Apecs.cmap $ \(CLerpPosition LerpPosition {..}, CPosition position) -> if position /= current then Right (CLerpPosition $ LerpPosition current position) else Left ()

      let drawFrame frames n =
            let index = n `mod` length frames
             in drawSprite renderer spritesheet (Just (toList frames !! index)) . toRect

      zipWithM_ (drawFrame edgeSprites) (randoms g) (toList edges)
      zipWithM_ (drawFrame groundSprites) (randoms g) (toList ground)
      drawSprite renderer spritesheet (Just goalSprite) (toRect $ SDL.V2 18 1)

      forM_ happenings $ \case
        Attack {..} -> do
          (player :: Maybe CPlayer, enemy :: Maybe CEnemy, CAnimation unchanged) <- Apecs.get source

          case (player, enemy) of
            (Just _, _) -> do
              playRandomChunk 0 chunksChop
              Apecs.set source $ CAnimation playerAttackAnimation
            (_, Just (CEnemy Zombie)) -> do
              playRandomChunk 2 chunksEnemy
              Apecs.set source $ CAnimation zombieAttackAnimation
              Apecs.modify source $ \(CPlayer, CAnimation _) -> CAnimation playerHurtAnimation
            (_, Just (CEnemy Vampire)) -> do
              playRandomChunk 2 chunksEnemy
              Apecs.set source $ CAnimation vampireAttackAnimation
              Apecs.modify source $ \(CPlayer, CAnimation _) -> CAnimation playerHurtAnimation
            _ -> Apecs.set source $ CAnimation unchanged
        Move {..} -> do
          playRandomChunk 0 chunksFootstep
          Apecs.modify source $ \(CPlayer, CAnimation _) -> CAnimation playerIdleAnimation
        Pickup {..} -> do
          (CFood food) <- Apecs.get target
          case food of
            Fruit -> playRandomChunk 1 chunksFruit
            Soda -> playRandomChunk 1 chunksSoda
        _ -> pass

      Apecs.cmapM_ $
        \(CPosition position, CEnemy enemy, CLife life, Apecs.Entity e) ->
          case enemy of
            Obstacle -> do
              let g' = mkStdGen (e + l)
              drawFrame (if life > 1 then obstacleSprites else brokenObstacleSprites) (fst $ random g') position
            _ -> pass

      Apecs.cmapM_ $ \(CPosition position, CFood pickup) -> do
        case pickup of
          Fruit -> drawFrame pickupSprites 0 position
          Soda -> drawFrame pickupSprites 1 position

      Apecs.cmapM $ \(CPosition position, CLerpPosition LerpPosition {..}, CAnimation animation, _ :: Apecs.Not CDead) -> do
        let animationDone reel = floor (view timer animation) >= length reel
            drawAnimation reel = drawSprite renderer spritesheet (Just (toList reel !! t)) (toRect position)
              where
                t = floor (view timer animation) `mod` length reel

        case view name animation of
          PlayerIdle -> do
            drawAnimation playerIdleSprites
            pure pass
          PlayerAttack -> do
            drawAnimation playerAttackSprites
            pure . when (animationDone playerAttackSprites) $ Left (CAnimation playerIdleAnimation)
          PlayerHurt -> do
            drawAnimation playerHurtSprites
            pure . when (animationDone playerHurtSprites) $ Left (CAnimation playerIdleAnimation)
          ZombieIdle -> do
            drawAnimation zombieIdleSprites
            pure pass
          ZombieAttack -> do
            drawAnimation zombieAttackSprites
            pure . when (animationDone playerAttackSprites) $ Left (CAnimation zombieIdleAnimation)
          VampireIdle -> do
            drawAnimation vampireIdleSprites
            pure pass
          VampireAttack -> do
            drawAnimation vampireAttackSprites
            pure . when (animationDone playerAttackSprites) $ Left (CAnimation vampireIdleAnimation)

      pass
