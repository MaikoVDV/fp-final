module Main where

import System.Environment (getArgs)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)

import Model.TypesState
import View.Main
import Controller.Update
import Controller.Input
import Assets

main :: IO ()
main = do
  args <- getArgs
  playerAnim <- loadPlayerAnimation
  screenSize <- getScreenSize

  let debugEnabled = "debug" `elem` args
      menuState = MenuState
        { menuPlayerAnim = playerAnim
        , menuDebugMode = debugEnabled
        , menuScreenSize = screenSize
        , menuFocus = 0
        , menuPage = MainMenu
        , menuCustomFiles = []
        , menuInput = ""
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
