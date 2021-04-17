{-# LANGUAGE RecordWildCards #-}

module Game.System.Logic (system) where

import Apecs qualified
import Apecs.Experimental.Reactive (ixLookup, withReactive)
import Control.Monad.Except (throwError)
import Data.List ((!!))
import Data.Set (member)
import Game.Level (cleanLevel, createLevel, createLevelOne, destroyComponents, edges, goal)
import Relude
import SDL qualified
import System.Random (Random (randomRIO))
import Types (Command (End))
import World (System')
import World.Component

pressed :: KeyboardInput -> Bool
pressed (KeyboardInput _ KeyPressed) = True
pressed _ = False

entitiesAt :: Position -> System' [Apecs.Entity]
entitiesAt pos = withReactive $ ixLookup (CPosition pos)

checkPlayerCollision :: Apecs.Entity -> SDL.V2 Int -> Maybe (SDL.V2 Int) -> System' (SDL.V2 Int, [Happening])
checkPlayerCollision _ from Nothing = pure (from, [])
checkPlayerCollision playerEntity from (Just movement)
  | to `member` edges = pure (from, [])
  | otherwise = entitiesAt to >>= go
  where
    to = from + movement
    foodLife Fruit = 30
    foodLife Soda = 20

    go :: [Apecs.Entity] -> System' (SDL.V2 Int, [Happening])
    go [] = pure (to, [Move playerEntity from to])
    go xs = do
      collision <-
        fmap (listToMaybe . sortBy (compare `on` fst) . catMaybes) . forM xs $
          Apecs.get
            >=> \case
              (Just (CEnemy _), _, enemyEntity) ->
                pure $ Just (from, [Attack playerEntity 1 enemyEntity])
              (_, Just (CFood food), foodEntity) ->
                pure $
                  Just
                    ( to,
                      [ Move playerEntity from to,
                        Pickup playerEntity (foodLife food) foodEntity
                      ]
                    )
              _ -> pure Nothing

      pure $ fromMaybe (from, []) collision

checkEnemyCollision :: Apecs.Entity -> SDL.V2 Int -> SDL.V2 Int -> System' (SDL.V2 Int, [Happening])
checkEnemyCollision enemyEntity from movement
  | to `member` edges = pure (from, [])
  | otherwise = entitiesAt to >>= go
  where
    to = from + movement

    go :: [Apecs.Entity] -> System' (SDL.V2 Int, [Happening])
    go [] = pure (to, [])
    go xs = do
      collision <-
        fmap (listToMaybe . catMaybes) . forM xs $
          Apecs.get
            >=> \case
              (Just CPlayer, playerEntity) -> pure $ Just (from, [Attack enemyEntity 10 playerEntity])
              _ -> pure Nothing
      pure $ fromMaybe (from, []) collision

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
  where
    add m x y = (x + y + m) `rem` m

changeScene :: Scene -> System' ()
changeScene = Apecs.set Apecs.global . CScene

system :: System' ()
system = do
  Apecs.cmap $ \CDead -> destroyComponents
  Apecs.set Apecs.global (CHappenings [])

  (CScene scene, CLevel (Level l)) <- Apecs.get Apecs.global
  case scene of
    GameOver GameOverInput {..} -> do
      when (pressed restart) $ do
        cleanLevel
        createLevelOne
        changeScene (LevelTitle defaultLevelTitleInput)
      when (pressed exit) $ do
        cleanLevel
        Apecs.set Apecs.global (CLevel (Level 0), CScene (LevelTitle defaultLevelTitleInput))
    MainMenu input@MainMenuInput {..} choice -> do
      when (pressed toGame) $ changeScene (Game defaultGameInput)
      when (pressed pressUp) $ changeScene (MainMenu input (turn (-1) choice))
      when (pressed pressDown) $ changeScene (MainMenu input (turn 1 choice))
      when (pressed selectOption) $ case choice of
        StartGame -> do
          if l == 0
            then do
              cleanLevel
              createLevelOne
              changeScene (LevelTitle defaultLevelTitleInput)
            else do
              changeScene (Game defaultGameInput)
        ExitGame -> lift . throwError $ End
    LevelTitle LevelTitleInput {..} -> do
      when (pressed continue) $ do
        changeScene (Game defaultGameInput)
    Game GameInput {..} -> do
      when (pressed toMenu) $ changeScene (MainMenu defaultMainMenuInput StartGame)

      let ifPressed b v = if pressed b then Just v else Nothing
          moveInput =
            listToMaybe . catMaybes $
              [ ifPressed moveLeft $ SDL.V2 (-1) 0,
                ifPressed moveRight $ SDL.V2 1 0,
                ifPressed moveUp $ SDL.V2 0 (-1),
                ifPressed moveDown $ SDL.V2 0 1
              ]

      Apecs.cmapM $ \(CPlayer, CPosition position, e :: Apecs.Entity) -> do
        (next, happenings) <- checkPlayerCollision e position moveInput
        let nextLevel = [NextLevel | position == goal]
        Apecs.modify Apecs.global (<> CHappenings (happenings <> nextLevel))
        pure . Just $ CPosition next

      playerAction <- (\(CHappenings hs) -> not . null $ hs) <$> Apecs.get Apecs.global

      when playerAction . Apecs.cmapM $ \(CEnemy enemy, CPosition position, e :: Apecs.Entity) -> do
        case enemy of
          Obstacle -> pure $ Left ()
          _ -> do
            let directions = [SDL.V2 (-1) 0, SDL.V2 1 0, SDL.V2 0 (-1), SDL.V2 0 1]
            index <- liftIO $ randomRIO (0, length directions - 1)

            (next, happenings) <- checkEnemyCollision e position (directions !! index)
            Apecs.modify Apecs.global (<> CHappenings happenings)
            pure . Right $ CPosition next

      (CHappenings happenings) <- Apecs.get Apecs.global

      forM_ happenings $ \case
        Attack {..} -> do
          Apecs.modify source $ \(CPlayer, CLife life) -> CLife (life - 1)
          Apecs.modify target $ \(CLife current) -> CLife (current - damage)
        Move {..} -> do
          Apecs.modify source $ \(CPlayer, CLife life) -> CLife (life - 1)
        Pickup {..} -> do
          Apecs.modify source $ \(CLife current) -> CLife (current + life)
          Apecs.set target CDead
        _ -> pass

      Apecs.cmap $ \(CLife current) ->
        if current <= 0
          then Right CDead
          else Left ()

      when (pressed resetLevel) $ do
        cleanLevel
        createLevel l 100

      when (NextLevel `elem` happenings) $ do
        Apecs.cmapM_ $ \(CPlayer, CLife life) -> do
          cleanLevel
          createLevel (l + 1) life
        changeScene (LevelTitle defaultLevelTitleInput)

      Apecs.cmapM_ $ \(CPlayer, CDead) ->
        changeScene (GameOver defaultGameOverInput)

  pass