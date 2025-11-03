module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model.Types
import View
import Controller.Update
import Controller.Input
import Assets
import System.Environment (getArgs)
import Graphics.Gloss.Interface.Environment (getScreenSize)

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
