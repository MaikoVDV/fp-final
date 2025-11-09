{-# LANGUAGE OverloadedStrings #-}

module Controller.Input where

import System.FilePath
import System.Directory
import Graphics.Gloss.Interface.IO.Game
import Data.List (isPrefixOf)
import Data.Maybe
import Control.Monad (when)

import Model.Types
import Model.TypesState
import Model.InitialState
import Model.Config 
import Model.WorldMap 

import Controller.InputMenu
import Controller.InputBuilder

import LevelCodec 
import MathUtils
import Assets

-- Press 's' in build mode to save the current level
-- Normally levels are saved to the levels folder, but when in debug mode, they're saved to built-in-levels
-- When in debug mode, 'p' saves the level as a segment to be used in the infinite scolling mode
input :: Event -> AppState -> IO AppState
input e appState =
  case appState of
    Menu menuState    -> handleMenuInput e menuState
    Playing gameState -> return . Playing $ handlePlayingInput e gameState
    Building bs       -> case e of
      -- Save current level
      EventKey (Char 's') Down _ _ -> do
        let baseQuick = if builderDebugMode bs then "built-in-levels/quicksave.lvl" else "levels/quicksave.lvl"
            mapToBuiltIn p
              | "levels/" `isPrefixOf` p = "built-in-levels/" ++ drop 7 p
              | "built-in-levels/" `isPrefixOf` p = p
              | otherwise = if builderDebugMode bs then "built-in-levels/" ++ p else p
            outPath0 = case builderFilePath bs of
              Just p  -> if builderDebugMode bs then mapToBuiltIn p else p
              Nothing -> baseQuick
            dir = takeDirectory outPath0
        createDirectoryIfMissing True dir
        saveBuilderLevel outPath0 bs
        putStrLn ("Saved level to " ++ outPath0)
        return (Building (bs { builderDirty = False }))
      -- Save current layout as an infinite-segment snippet (debug builder only)
      EventKey (Char 'p') Down _ _ | builderDebugMode bs -> do
        saveBuilderSegment bs
        return (Building bs)
      -- Escape: if dirty prompt, otherwise leave to menu
      EventKey (SpecialKey KeyEsc) Down _ _ ->
        if builderConfirmLeave bs
          then return (Building (bs { builderConfirmLeave = False }))
          else if builderDirty bs
            then return (Building (bs { builderConfirmLeave = True }))
            else leaveToMenuFromBuilder bs
      -- Popup keyboard shortcuts
      EventKey (Char 'y') Down _ _ | builderConfirmLeave bs -> leaveToMenuFromBuilder bs
      EventKey (Char 'Y') Down _ _ | builderConfirmLeave bs -> leaveToMenuFromBuilder bs
      EventKey (Char 'n') Down _ _ | builderConfirmLeave bs -> return (Building (bs { builderConfirmLeave = False }))
      EventKey (Char 'N') Down _ _ | builderConfirmLeave bs -> return (Building (bs { builderConfirmLeave = False }))
      -- Popup mouse click on Yes/No
      EventKey (MouseButton LeftButton) Down _ (mx, my) | builderConfirmLeave bs ->
        case leaveConfirmHit (builderScreenSize bs) (mx, my) of
          Just True  -> leaveToMenuFromBuilder bs
          Just False -> return (Building (bs { builderConfirmLeave = False }))
          Nothing    -> return (Building bs)
      -- Otherwise, normal builder input unless popup visible
      _ -> if builderConfirmLeave bs
              then return (Building bs)
              else return . Building $ handleBuildingInput e bs
    WorldMapScreen ms -> handleWorldMapInput e ms

startGame :: MenuState -> IO AppState
startGame menuState = do
  initialState <- buildInitialGameState menuState
  when (menuDebugMode menuState) $
    print (colliders (world initialState))
  return (Playing initialState)

-- Basic world map input: Esc -> back to menu; Enter -> start default game
handleWorldMapInput :: Event -> MapState -> IO AppState
handleWorldMapInput e ms@MapState { wmWorldMap = wm, wmCursor = cur, wmAlong } = case e of
  EventKey (SpecialKey KeyEsc) Down _ _   -> return (Menu (wmMenuState ms))
  EventKey (SpecialKey KeyEnter) Down _ _ -> startCurrentNodeLevel ms
  EventKey (Char 'w') Down _ _ | isNothing wmAlong -> return (tryMove (0,1))
  EventKey (Char 'W') Down _ _ | isNothing wmAlong -> return (tryMove (0,1))
  EventKey (Char 's') Down _ _ | isNothing wmAlong -> return (tryMove (0,-1))
  EventKey (Char 'S') Down _ _ | isNothing wmAlong -> return (tryMove (0,-1))
  EventKey (Char 'a') Down _ _ | isNothing wmAlong -> return (tryMove (-1,0))
  EventKey (Char 'A') Down _ _ | isNothing wmAlong -> return (tryMove (-1,0))
  EventKey (Char 'd') Down _ _ | isNothing wmAlong -> return (tryMove (1,0))
  EventKey (Char 'D') Down _ _ | isNothing wmAlong -> return (tryMove (1,0))
  _ -> return (WorldMapScreen ms)

  where
    tryMove :: (Float, Float) -> AppState
    tryMove dir =
      let options = adjacentDirected wm cur
          withScore = [ (o, dot v dir) | o@(_,_,_,v) <- options ]
          positive = filter ((> 0) . snd) withScore
      in case positive of
          [] -> WorldMapScreen ms
          _  -> let (bestO, _) = maximumBySnd positive
                    (edge, nb, pts, _) = bestO
                in WorldMapScreen (ms { wmAlong = Just (edgeId edge, pts, nb, 0) })

    dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

    -- Finds the max value in an array of tuples, sorted by the snd value.
    maximumBySnd :: Ord b => [(a,b)] -> (a,b)
    maximumBySnd = foldl1 (\acc@(_,b1) x@(_,b2) -> if b2 > b1 then x else acc)

startCurrentNodeLevel :: MapState -> IO AppState
startCurrentNodeLevel ms@MapState { wmWorldMap = wm, wmCursor = nid, wmMenuState = m } =
  case nodeById wm nid >>= levelRef of
    Nothing -> return (WorldMapScreen ms)
    Just ref -> do
      let path = case ref of
            BuiltIn p  -> p
            External p -> p
          debugEnabled = menuDebugMode m
          screenDims   = menuScreenSize m
      tileMap <- loadTileMap
      gs <- loadLevel path debugEnabled tileMap screenDims
      let gs' = gs { currentMapState = Just ms }
      return (Playing gs')


handlePlayingInput :: Event -> GameState -> GameState
-- Pause toggle on ESC
handlePlayingInput (EventKey (SpecialKey KeyEsc) Down _ _) gs = gs { paused = not (paused gs) }
-- While paused: UI interactions for pause menu
handlePlayingInput (EventKey (Char 'r') Down _ _) gs | paused gs = gs { paused = False }
handlePlayingInput (EventKey (Char 'R') Down _ _) gs | paused gs = gs { paused = False }
handlePlayingInput (EventKey (Char 'm') Down _ _) gs | paused gs = gs { nextState = NMenu }
handlePlayingInput (EventKey (Char 'M') Down _ _) gs | paused gs = gs { nextState = NMenu }
handlePlayingInput (EventKey (MouseButton LeftButton) Down _ (mx, my)) gs@GameState { screenSize } | paused gs =
  case pausedMenuHit screenSize (mx, my) of
    Just True  -> gs { paused = False }
    Just False -> gs { nextState = NMenu }
    Nothing    -> gs
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

-- Hit test for leave confirmation popup buttons
leaveConfirmHit :: (Int, Int) -> (Float, Float) -> Maybe Bool
leaveConfirmHit (screenW, screenH) (mx, my) =
  let sw = fromIntegral screenW :: Float
      sh = fromIntegral screenH :: Float
      panelW = sw * 0.6
      panelH = sh * 0.3
      btnW = 220 :: Float
      btnH = 80  :: Float
      btnY = - panelH * 0.15
      yesX = - panelW * 0.2
      noX  =   panelW * 0.2
      inside cx cy w h = let dx = abs (mx - cx); dy = abs (my - cy) in dx <= w/2 && dy <= h/2
  in if inside yesX btnY btnW btnH then Just True
     else if inside noX btnY btnW btnH then Just False
     else Nothing

leaveToMenuFromBuilder :: BuilderState -> IO AppState
leaveToMenuFromBuilder bs = do
  anim <- loadPlayerAnimation
  let ms = MenuState
            { menuPlayerAnim = anim
            , menuDebugMode = builderDebugMode bs
            , menuScreenSize = builderScreenSize bs
            , menuFocus = 0
            , menuPage = MainMenu
            , menuCustomFiles = []
            , menuInput = ""
            }
  return (Menu ms)

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

-- Pause menu hit test
pausedMenuHit :: (Int, Int) -> (Float, Float) -> Maybe Bool
pausedMenuHit (screenW, screenH) (mx, my) =
  let sw = fromIntegral screenW :: Float
      sh = fromIntegral screenH :: Float
      panelW = sw * 0.6
      panelH = sh * 0.3
      btnW = 260 :: Float
      btnH = 90  :: Float
      btnY = 0 - panelH * 0.15
      resumeX = - panelW * 0.2
      menuX   =   panelW * 0.2
      inside cx cy w h = let dx = abs (mx - cx); dy = abs (my - cy) in dx <= w/2 && dy <= h/2
  in if inside resumeX btnY btnW btnH then Just True
     else if inside menuX btnY btnW btnH then Just False
     else Nothing
