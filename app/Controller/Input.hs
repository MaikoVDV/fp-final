module Controller.Input where

import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
-- import System.Directory (createDirectoryIfMissing)
import Control.Monad (when)

import Model.Types
-- import LevelCodec
import MathUtils
import Model.InitialState
import Model.Config

input :: Event -> AppState -> IO AppState
input (EventKey (SpecialKey KeyEsc) Down _ _) appState = exitSuccess >> return appState
-- Ctrl+S quick-save current level to levels/quicksave.lvl
input e appState =
  case appState of
    Menu menuState  -> handleMenuInput e menuState
    Playing gameState -> return . Playing $ handlePlayingInput e gameState

handleMenuInput :: Event -> MenuState -> IO AppState
handleMenuInput (EventKey (SpecialKey KeyEnter) Down _ _) menuState = startGame menuState
handleMenuInput _ menuState = return (Menu menuState)

startGame :: MenuState -> IO AppState
startGame menuState = do
  initialState <- buildInitialGameState menuState
  when (menuDebugMode menuState) $
    print (colliders (world initialState))
  return (Playing initialState)

handlePlayingInput :: Event -> GameState -> GameState
-- Movement: arrows and WASD
handlePlayingInput (EventKey (SpecialKey KeyLeft)  Down _ _) gs = gs { player = (player gs) { moveLeftHeld  = True } }
handlePlayingInput (EventKey (SpecialKey KeyRight) Down _ _) gs = gs { player = (player gs) { moveRightHeld = True } }
handlePlayingInput (EventKey (SpecialKey KeyLeft)  Up   _ _) gs = gs { player = (player gs) { moveLeftHeld  = False } }
handlePlayingInput (EventKey (SpecialKey KeyRight) Up   _ _) gs = gs { player = (player gs) { moveRightHeld = False } }
handlePlayingInput (EventKey (Char 'a') Down _ _) gs = gs { player = (player gs) { moveLeftHeld  = True } }
handlePlayingInput (EventKey (Char 'A') Down _ _) gs = gs { player = (player gs) { moveLeftHeld  = True } }
handlePlayingInput (EventKey (Char 'a') Up   _ _) gs = gs { player = (player gs) { moveLeftHeld  = False } }
handlePlayingInput (EventKey (Char 'A') Up   _ _) gs = gs { player = (player gs) { moveLeftHeld  = False } }
handlePlayingInput (EventKey (Char 'd') Down _ _) gs = gs { player = (player gs) { moveRightHeld = True } }
handlePlayingInput (EventKey (Char 'D') Down _ _) gs = gs { player = (player gs) { moveRightHeld = True } }
handlePlayingInput (EventKey (Char 'd') Up   _ _) gs = gs { player = (player gs) { moveRightHeld = False } }
handlePlayingInput (EventKey (Char 'D') Up   _ _) gs = gs { player = (player gs) { moveRightHeld = False } }
-- Zoom controls
handlePlayingInput (EventKey (Char '+') Down _ _) gs = adjustTileZoom zoomStep gs
handlePlayingInput (EventKey (Char '=') Down _ _) gs = adjustTileZoom zoomStep gs
handlePlayingInput (EventKey (Char '-') Down _ _) gs = adjustTileZoom (-zoomStep) gs
handlePlayingInput (EventKey (Char '_') Down _ _) gs = adjustTileZoom (-zoomStep) gs
-- Jump: Up arrow and W
handlePlayingInput (EventKey (SpecialKey KeyUp)    Down _ _) gs = gs { pendingJump = True, jumpHeld = True }
handlePlayingInput (EventKey (SpecialKey KeyUp)    Up   _ _) gs = gs { jumpHeld = False }
handlePlayingInput (EventKey (Char 'w')            Down _ _) gs = gs { pendingJump = True, jumpHeld = True }
handlePlayingInput (EventKey (Char 'W')            Down _ _) gs = gs { pendingJump = True, jumpHeld = True }
handlePlayingInput (EventKey (Char 'w')            Up   _ _) gs = gs { jumpHeld = False }
handlePlayingInput (EventKey (Char 'W')            Up   _ _) gs = gs { jumpHeld = False }
handlePlayingInput (EventKey (SpecialKey KeySpace) Down _ _) gs = gs { pendingJump = True, jumpHeld = True }
handlePlayingInput (EventKey (SpecialKey KeySpace) Up   _ _) gs = gs { jumpHeld = False }
-- Sprint: Left Shift (avoid Ctrl + Char issues on Windows/GLUT)
handlePlayingInput (EventKey (SpecialKey KeyShiftL) Down _ _) gs = gs { sprintHeld = True }
handlePlayingInput (EventKey (SpecialKey KeyShiftL) Up   _ _) gs = gs { sprintHeld = False }
handlePlayingInput _ gs = gs


computeJumpHold :: Float -> GameState -> Player -> (Vector, Float)
computeJumpHold dt gs Player { playerJumpTime = t0, onGround = grounded, playerJumpDir = initDir }
  | not (jumpHeld gs)        = ((0, 0), jumpHoldDuration)
  | grounded                 = ((0, 0), jumpHoldDuration)
  | t0 >= jumpHoldDuration   = ((0, 0), jumpHoldDuration)
  | otherwise =
      let startTime   = clampInterval t0
          endTime     = min jumpHoldDuration (startTime + dt)
          accelStart  = jumpHoldValue startTime
          accelEnd    = jumpHoldValue endTime
          dirStart    = jumpDirectionAt initDir startTime
          dirEnd      = jumpDirectionAt initDir endTime
          accelVecStart = scaleVec dirStart accelStart
          accelVecEnd   = scaleVec dirEnd   accelEnd
          avgAccelVec   = scaleVec (addVec accelVecStart accelVecEnd) 0.5
      in (avgAccelVec, endTime)
  where
    clampInterval t = max 0 (min jumpHoldDuration t)

jumpHoldValue :: Float -> Float
jumpHoldValue t
  | jumpHoldDuration <= 0 = 0
  | otherwise =
      let factor = max 0 (1 - t / jumpHoldDuration)
      in jumpHoldAccelStart * factor

jumpDirectionAt :: Vector -> Float -> Vector
jumpDirectionAt initDir t
  | jumpHoldDuration <= 0 = upVector
  | otherwise =
      let alpha = clamp01 (t / jumpHoldDuration)
          blended = addVec (scaleVec initDir (1 - alpha)) (scaleVec upVector alpha)
      in normalizeVec blended

-- Adjust zoom by changing tileZoom, clamped to [minTileZoom, maxTileZoom]
adjustTileZoom :: Float -> GameState -> GameState
adjustTileZoom delta gs =
  let cur = tileZoom gs
      newVal = clampTileZoom (cur + delta)
  in gs { tileZoom = newVal }
