{-# LANGUAGE RecordWildCards #-}

module Game.System.Input (system) where

import Apecs qualified
import Control.Monad.Except (throwError)
import Relude
import Relude.Extra (member)
import SDL qualified
import Types (Command (End), Tick (readEvents))
import World (System', tickState)
import World.Component
  ( CScene (..),
    GameInput (..),
    GameOverInput (..),
    KeyboardInput (..),
    KeyboardState (KeyDown, KeyPressed, KeyReleased, KeyUp),
    LevelTitleInput (..),
    MainMenuInput (..),
    Scene (..),
  )

parseKeyboardInput :: (Set SDL.Scancode, Set SDL.Scancode) -> (SDL.Scancode -> Bool) -> KeyboardInput -> KeyboardInput
parseKeyboardInput (pressedKeys, releasedKeys) isKeyDown (KeyboardInput scancode _) =
  let keyPressed = scancode `member` pressedKeys
      keyReleased = scancode `member` releasedKeys
      keyDown = isKeyDown scancode
   in KeyboardInput scancode $ case (keyPressed, keyReleased, keyDown) of
        (True, _, _) -> KeyPressed
        (_, True, _) -> KeyReleased
        (_, _, True) -> KeyDown
        _ -> KeyUp

updateGameInput :: GameInput -> (KeyboardInput -> KeyboardInput) -> GameInput
updateGameInput GameInput {..} update =
  GameInput
    { moveRight = update moveRight,
      moveLeft = update moveLeft,
      moveUp = update moveUp,
      moveDown = update moveDown,
      toMenu = update toMenu,
      resetLevel = update resetLevel
    }

updateMainMenuInput :: MainMenuInput -> (KeyboardInput -> KeyboardInput) -> MainMenuInput
updateMainMenuInput MainMenuInput {..} update =
  MainMenuInput
    { pressUp = update pressUp,
      pressDown = update pressDown,
      selectOption = update selectOption,
      toGame = update toGame
    }

updateLevelTitleInput :: LevelTitleInput -> (KeyboardInput -> KeyboardInput) -> LevelTitleInput
updateLevelTitleInput LevelTitleInput {..} update =
  LevelTitleInput
    { continue = update continue
    }

updateGameOverInput :: GameOverInput -> (KeyboardInput -> KeyboardInput) -> GameOverInput
updateGameOverInput GameOverInput {..} update =
  GameOverInput
    { restart = update restart,
      exit = update exit
    }

separateMotion :: SDL.KeyboardEventData -> Either SDL.Scancode SDL.Scancode
separateMotion e = if pressed then Left scancode else Right scancode
  where
    pressed = SDL.keyboardEventKeyMotion e == SDL.Pressed
    scancode = SDL.keysymScancode (SDL.keyboardEventKeysym e)

keyboardEventData :: SDL.EventPayload -> Maybe SDL.KeyboardEventData
keyboardEventData (SDL.KeyboardEvent e) = Just e
keyboardEventData _ = Nothing

applyBoth :: (t -> b) -> (t, t) -> (b, b)
applyBoth f (a, b) = (f a, f b)

isQuitEvent :: SDL.Event -> Bool
isQuitEvent (SDL.Event _t SDL.QuitEvent) = True
isQuitEvent _ = False

system :: System' ()
system = do
  events <- tickState readEvents
  when (any isQuitEvent events) . lift . throwError $ End

  isKeyDown <- SDL.getKeyboardState
  let payload = SDL.eventPayload <$> events
      keyboardEvents = mapMaybe keyboardEventData payload
      keySets = applyBoth (fromList @(Set SDL.Scancode)) . partitionEithers $ separateMotion <$> keyboardEvents
      update = parseKeyboardInput keySets isKeyDown
  (CScene schema) <- Apecs.get Apecs.global
  let updatedSchema = case schema of
        Game input -> Game $ updateGameInput input update
        GameOver input -> GameOver $ updateGameOverInput input update
        MainMenu input menuChoice -> MainMenu (updateMainMenuInput input update) menuChoice
        LevelTitle input -> LevelTitle $ updateLevelTitleInput input update
  Apecs.set Apecs.global (CScene updatedSchema)

  pass