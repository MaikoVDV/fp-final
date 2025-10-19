module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model
import View
import Controller
import Assets
import System.Environment (getArgs)
import Graphics.Gloss.Interface.Environment (getScreenSize)

main :: IO ()
main = do
  args <- getArgs
  tileMap <- loadTileMap
  playerSprite <- loadPlayerSprite
  screenSize <- getScreenSize

  let debugEnabled = "debug" `elem` args
      menuState = MenuState
        { menuTileMap = tileMap
        , menuPlayerSprite = playerSprite
        , menuDebugMode = debugEnabled
        , menuScreenSize = screenSize
        }
      initialState = Menu menuState

  playIO
    FullScreen
    (makeColorI 135 206 235 255)
    60
    initialState
    view
    input
    update
