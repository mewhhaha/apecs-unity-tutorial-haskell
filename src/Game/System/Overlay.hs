{-# LANGUAGE RecordWildCards #-}

module Game.System.Overlay (system) where

import Apecs qualified
import Foreign.C (CInt)
import Game.Resource (resourceFont)
import Relude
import SDL qualified
import SDL.Font qualified as SDLFont
import System.FilePath ((</>))
import Types (Tick (readWindow), readRenderer)
import World (System', tickState)
import World.Component

data Painter
  = At {position :: SDL.V2 CInt, content :: Painter}
  | Row {columns :: [Painter]}
  | Column {rows :: [Painter]}
  | Painter {paint :: SDL.V2 CInt -> System' (SDL.V2 CInt)}

data FontAlign = FontAbove | FontMiddle | FontBelow

data FontJustify = FontLeft | FontCenter | FontRight

data TextConfig = TextConfig
  { fontFamily :: String,
    fontSize :: Int,
    fontColor :: SDL.V4 Word8,
    fontAlign :: FontAlign,
    fontJustify :: FontJustify
  }

defaultText :: TextConfig
defaultText =
  TextConfig
    { fontFamily = "PressStart2P-Regular.ttf",
      fontSize = 16,
      fontColor = maxBound,
      fontAlign = FontBelow,
      fontJustify = FontLeft
    }

text :: TextConfig -> Text -> Painter
text TextConfig {..} t = Painter $ \origin -> do
  renderer <- tickState readRenderer
  font <- resourceFont ("fonts" </> fontFamily) fontSize
  surface <- SDLFont.blended font fontColor t
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  info <- SDL.queryTexture texture
  let size@(SDL.V2 w h) = SDL.V2 (SDL.textureWidth info) (SDL.textureHeight info)
      offsetX = case fontJustify of
        FontLeft -> 0
        FontRight -> (- w)
        FontCenter -> (- w `div` 2)

      offsetY = case fontAlign of
        FontBelow -> 0
        FontAbove -> - h
        FontMiddle -> (- h `div` 2)
      offset = SDL.V2 offsetX offsetY
  SDL.copy renderer texture Nothing (Just (SDL.Rectangle (SDL.P $ fromIntegral <$> (origin + offset)) size))
  SDL.destroyTexture texture
  pure size

runPainter :: Painter -> System' ()
runPainter painter = do
  _ <- go SDL.zero painter
  pass
  where
    go origin Painter {paint} = paint origin
    go _ At {position, content} = go position content
    go origin Column {rows} =
      let eval rowOrigin [] = pure rowOrigin
          eval rowOrigin (row : rs) = do
            (SDL.V2 _ h) <- go rowOrigin row
            eval (rowOrigin + SDL.V2 0 h) rs
       in eval origin rows
    go origin Row {columns} =
      let eval columnOrigin [] = pure columnOrigin
          eval columnOrigin (row : rs) = do
            (SDL.V2 w _) <- go columnOrigin row
            eval (columnOrigin + SDL.V2 w 0) rs
       in eval origin columns

system :: System' ()
system = do
  window <- tickState readWindow
  (SDL.V2 w h) <- SDL.get (SDL.windowSize window)

  (CScene inputSchema, CLevel (Level l)) <- Apecs.get Apecs.global

  let centeredText = text defaultText {fontJustify = FontCenter, fontAlign = FontMiddle}
  let menuText fontColor = text defaultText {fontJustify = FontCenter, fontColor = fontColor}
  case inputSchema of
    MainMenu MainMenuInput {} choice -> do
      let selectColor = SDL.V4 0 255 0 0
      let unselectColor = maxBound
      let (startColor, exitColor) = case choice of
            StartGame -> (selectColor, unselectColor)
            ExitGame -> (unselectColor, selectColor)
      let startText = if l == 0 then "Start game" else "Continue game"
      runPainter $
        At
          (SDL.V2 (w `div` 2) (h `div` 2))
          ( Column
              [ menuText startColor startText,
                menuText exitColor "Exit game"
              ]
          )
    GameOver GameOverInput {} -> do
      runPainter $ At (SDL.V2 (w `div` 2) (h `div` 2)) (Column [centeredText ("You died at level " <> show l), centeredText "Press [Enter] to restart"])
    LevelTitle LevelTitleInput {} -> do
      runPainter $ At (SDL.V2 (w `div` 2) (h `div` 2)) (Column [centeredText ("Level " <> show l), centeredText "Press [Enter] to start"])
    Game GameInput {} -> do
      Apecs.cmapM_ $ \(CPlayer, CLife life) ->
        runPainter $ At (SDL.V2 (w `div` 2) h) (Row [text defaultText {fontJustify = FontCenter, fontAlign = FontAbove} ("Food " <> show life)])

  pass
