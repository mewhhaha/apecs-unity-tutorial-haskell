{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Draw
  ( draw,
  )
where

import Apecs
  ( Get,
    Members,
    Not (..),
    cfold,
    cfoldM_,
    cmapM_,
    get,
    global,
    lift,
    newEntity,
  )
import Control.Monad (unless, void, when, zipWithM_)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.StateVar as StateVar
import qualified Data.Text as Text
import Engine.SDL (render)
import Engine.SDL.Internal (ASheet, Drawable, Sheet (..), TextElement (..), Texture (..), XAlignment (..), animate, linear, loadTexture, mkASheet, mkClips, mkPoint, mkRect, mkSheet, mkTextElement)
import qualified Env
import GHC.TypeNats
import Game.Component
import Game.World (System', World)
import Helper.Happened (isEnemyAttack, isFruitPicked, isPlayerAttack, isPlayerDie, isPlayerMove, isSodaPicked)
import Linear ((*^), V2 (..), V4 (..), (^+^), (^-^))
import qualified SDL
import qualified SDL.Font
import qualified SDL.Mixer
import qualified SDL.Video
import System.Random (newStdGen, randomR)

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

maybeIf :: (a -> Bool) -> a -> Maybe a
maybeIf f a = if f a then Just a else Nothing

collectFood :: [Happened] -> Maybe Integer
collectFood =
  maybeIf (/= 0)
    . foldl'
      ( \acc happened -> case happened of
          FruitPicked x -> acc + fromIntegral x
          SodaPicked x -> acc + fromIntegral x
          PlayerHurt x -> acc - fromIntegral x
          _ -> acc
      )
      0

getTextOffset :: Integral i => TextElement -> i
getTextOffset (TextElement alignment (Texture _ ti)) = case alignment of
  XLeft -> 0
  XCenter -> - tw `div` 2
  XRight -> - tw
  where
    tw = fromIntegral $ SDL.textureWidth ti

stepUI :: SDL.Renderer -> Map.Map SDL.Font.PointSize SDL.Font.Font -> System' ()
stepUI r font = do
  updateGameOverlay
  updateLevelOverlay
  updateDeathOverlay
  where
    (Just f) = Map.lookup 16 font
    updateGameOverlay :: System' ()
    updateGameOverlay = do
      (CLatest latest) <- get global
      let shouldUpdate = (not . null) latest
      uninitialized <- cfold (\_ (_ :: CGameOverlay) -> False) True
      updated <- do
        food <- cfold (\_ (CPlayer _, CStat Stat {life}) -> life) 0
        let prefix = (\x -> "(" <> (if x >= 0 then "+" else mempty) <> show x <> ")") <$> collectFood latest
            foodText = Text.pack (fromMaybe mempty prefix <> " Food " <> show food)
        el <- mkTextElement r f XCenter foodText
        pure $ CGameOverlay (GameOverlay el)
      when (shouldUpdate || uninitialized) . void $ newEntity updated
    updateLevelOverlay :: System' ()
    updateLevelOverlay = do
      uninitialized <- cfold (\_ (_ :: CLevelOverlay) -> False) True
      when uninitialized $ do
        (CLevel level) <- get global
        let levelText = Text.pack ("Level " <> show level)
            enterText = Text.pack "Press [Enter]"
        els <- mapM (mkTextElement r f XCenter) [levelText, enterText]
        void $ newEntity (CLevelOverlay els)
    updateDeathOverlay :: System' ()
    updateDeathOverlay = do
      (CLatest latest) <- get global
      when (any isPlayerDie latest) $ do
        (CLevel level) <- get global
        let deathText = Text.pack ("You starved to death on level " <> show level)
            enterText = Text.pack "Press [Enter]"
        els <- mapM (mkTextElement r f XCenter) [deathText, enterText]
        void $ newEntity (CDeathOverlay els)

drawUI :: SDL.Window -> SDL.Renderer -> System' ()
drawUI w r = do
  screenSize@(V2 sw sh) <- StateVar.get $ SDL.Video.windowSize w
  cmapM_ $ \(CGameOverlay (GameOverlay el), game :: CGame) -> when (isGamePlay game) (renderText (sw `div` 2, sh - 23) el)
  cmapM_ $ \(CLevelOverlay els, game :: CGame) -> when (isLevelStart game) $ textOnBlack screenSize els
  cmapM_ $ \(CDeathOverlay els, game :: CGame) -> when (isGameOver game) $ textOnBlack screenSize els
  where
    isLevelStart LevelStart = True
    isLevelStart _ = False
    isGameOver GameOver = True
    isGameOver _ = False
    isGamePlay GamePlay = True
    isGamePlay _ = False
    getTextHeight (TextElement _ (Texture _ ti)) = SDL.textureHeight ti
    renderText :: (Integral i, MonadIO m) => (i, i) -> TextElement -> m ()
    renderText (x, y) el = render r (V2 (x + fromIntegral (getTextOffset el)) y) el
    clearBlack :: System' ()
    clearBlack = do
      let black = V4 0 0 0 0
      SDL.rendererDrawColor r SDL.$= black
      SDL.clear r
    rows :: forall i. Integral i => V2 i -> [TextElement] -> System' ()
    rows (V2 sw sh) = zipWithM_ (\n -> row <$> fromIntegral . (n *) . getTextHeight <*> id) [0 ..]
      where
        row :: i -> TextElement -> System' ()
        row rowOffset = renderText (sw `div` 2, sh `div` 2 + rowOffset)
    textOnBlack :: Integral i => V2 i -> [TextElement] -> System' ()
    textOnBlack screenSize els = do
      clearBlack
      rows screenSize els

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
  sequence_
    [ drawSheet ground,
      drawSheet wall,
      drawObstacle obstacle,
      drawSheet prop,
      drawPlayer player,
      drawVampire vampire,
      drawZombie zombie
    ]
  stepUI r font
  drawUI w r
  where
    volume :: SDL.Mixer.Volume
    volume = 20
    playMusic :: ByteString -> System' ()
    playMusic music = lift $ do
      SDL.Mixer.setMusicVolume 20
      decoded <- SDL.Mixer.decode music
      SDL.Mixer.playMusic SDL.Mixer.Once decoded
    playSound :: SDL.Mixer.Channel -> [SDL.Mixer.Chunk] -> System' ()
    playSound ch chunks = do
      g <- lift newStdGen
      let randomIndex = fst (randomR (0, length chunks - 1) g)
      SDL.Mixer.setVolume volume ch
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
    playEnemyAttack Env.Enemy {sfxAttack} = playSound enemySound sfxAttack
    playSoda :: Env.Misc -> System' ()
    playSoda Env.Misc {sfxSoda} = playSound miscSound sfxSoda
    playFruit :: Env.Misc -> System' ()
    playFruit Env.Misc {sfxFruit} = playSound miscSound sfxFruit
    toScreen :: Integral a => V2 Double -> V2 a
    toScreen p = floor . (* 32) <$> p
    interpolate :: CMove -> V2 Double
    interpolate (CMove t from to) = (fromIntegral <$> from) ^+^ (min (t / duration) 1 *^ (fromIntegral <$> (to ^-^ from)))
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
    drawCreature :: forall c. (Members World IO c, Get World IO c) => (CMove -> Double -> Double -> c -> System' ()) -> System' ()
    drawCreature f = cmapM_ $ \(c :: c, _ :: Not CDead, move :: CMove, CAnimation time dur) -> f move time dur c
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
