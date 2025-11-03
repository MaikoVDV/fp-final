module Controller.Input where

import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist)
import Control.Monad (when)

import Model.Types
import LevelCodec
import MathUtils
import Model.InitialState
import Model.Config
import Model.World
import Assets
import qualified Data.Map as Map
import Model.Entity (defaultPlayer)

input :: Event -> AppState -> IO AppState
-- Press 's' in build mode to save the current level
input e appState =
  case appState of
    Menu menuState    -> handleMenuInput e menuState
    Playing gameState -> return . Playing $ handlePlayingInput e gameState
    Building bs       -> case e of
      EventKey (Char 's') Down _ _ -> do
        let outPath = case builderFilePath bs of
              Just p  -> p
              Nothing -> "levels/quicksave.lvl"
        createDirectoryIfMissing True "levels"
        saveBuilderLevel outPath bs
        putStrLn ("Saved level to " ++ outPath)
        return (Building bs)
      _ -> return . Building $ handleBuildingInput e bs

-- Helper: convert BuilderState to a minimal GameState and save via LevelCodec
saveBuilderLevel :: FilePath -> BuilderState -> IO ()
saveBuilderLevel path bs = do
  let w = builderWorld bs
      gs = GameState
        { world = w
        , player = defaultPlayer
        , entities = []
        , entityIdCounter = 0
        , tileMap = builderTileMap bs
        , animMap = Map.empty
        , tileZoom = builderTileZoom bs
        , screenSize = builderScreenSize bs
        , frameCount = 0
        , paused = False
        , debugMode = builderDebugMode bs
        , pendingJump = False
        , jumpHeld = False
        , sprintHeld = False
        , menuState = MenuState { menuPlayerAnim = [], menuDebugMode = builderDebugMode bs, menuScreenSize = builderScreenSize bs, menuFocus = 0, menuPage = MainMenu, menuCustomFiles = [], menuInput = "" }
        , nextState = NMenu
        }
  saveLevel path gs

handleMenuInput :: Event -> MenuState -> IO AppState
handleMenuInput e ms@MenuState { menuFocus, menuPage } = case menuPage of
  MainMenu -> case e of
    -- Quit from main menu on ESC
    EventKey (SpecialKey KeyEsc) Down _ _ -> exitSuccess >> return (Menu ms)
    -- Keyboard focus navigation (W/S)
    EventKey (Char 'w') Down _ _ -> return . Menu $ ms { menuFocus = max 0 (menuFocus - 1) }
    EventKey (Char 'W') Down _ _ -> return . Menu $ ms { menuFocus = max 0 (menuFocus - 1) }
    EventKey (Char 's') Down _ _ -> return . Menu $ ms { menuFocus = min 2 (menuFocus + 1) }
    EventKey (Char 'S') Down _ _ -> return . Menu $ ms { menuFocus = min 2 (menuFocus + 1) }

    -- Activate focused button
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      case menuFocus of
        0 -> startGame ms
        1 -> goBuilderSelect ms
        2 -> goCustomLevels ms
        _ -> return (Menu ms)

    -- Mouse hover sets focus
    EventMotion (mx, my) ->
      case buttonFromMouseMain (mx, my) of
        Just f  -> return . Menu $ ms { menuFocus = f }
        Nothing -> return (Menu ms)

    -- Mouse click activates if inside button
    EventKey (MouseButton LeftButton) Down _ (mx, my) ->
      case buttonFromMouseMain (mx, my) of
        Just 0 -> startGame ms
        Just 1 -> goBuilderSelect ms
        Just 2 -> goCustomLevels ms
        _      -> return (Menu ms)

    _ -> return (Menu ms)

  CustomLevels -> case e of
    -- Navigate list
    EventKey (Char 'w') Down _ _ -> return . Menu $ ms { menuFocus = max 0 (menuFocus - 1) }
    EventKey (Char 'W') Down _ _ -> return . Menu $ ms { menuFocus = max 0 (menuFocus - 1) }
    EventKey (Char 's') Down _ _ ->
      let lastIdx = max 0 (length (menuCustomFiles ms) - 1)
      in return . Menu $ ms { menuFocus = min lastIdx (menuFocus + 1) }
    EventKey (Char 'S') Down _ _ ->
      let lastIdx = max 0 (length (menuCustomFiles ms) - 1)
      in return . Menu $ ms { menuFocus = min lastIdx (menuFocus + 1) }

    -- Activate focused item
    EventKey (SpecialKey KeyEnter) Down _ _ -> do
      case drop menuFocus (menuCustomFiles ms) of
        (f:_) -> startCustomLevel ms ("levels/" ++ f)
        _     -> return (Menu ms)

    -- Back to main menu
    EventKey (SpecialKey KeyEsc) Down _ _ -> return . Menu $ ms { menuPage = MainMenu, menuFocus = 0 }

    -- Mouse hover focus
    EventMotion (mx, my) ->
      case buttonFromMouseCustom ms (mx, my) of
        Just f  -> return . Menu $ ms { menuFocus = f }
        Nothing -> return (Menu ms)

    -- Mouse click
    EventKey (MouseButton LeftButton) Down _ (mx, my) ->
      case buttonFromMouseCustom ms (mx, my) of
        Just idx -> case drop idx (menuCustomFiles ms) of
          (f:_) -> startCustomLevel ms ("levels/" ++ f)
          _     -> return (Menu ms)
        Nothing -> return (Menu ms)

    _ -> return (Menu ms)

  BuilderSelect -> case e of
    -- Navigate (0 = New Level, 1.. = files)
    EventKey (Char 'w') Down _ _ -> return . Menu $ ms { menuFocus = max 0 (menuFocus - 1) }
    EventKey (Char 'W') Down _ _ -> return . Menu $ ms { menuFocus = max 0 (menuFocus - 1) }
    EventKey (Char 's') Down _ _ ->
      let lastIdx = length (menuCustomFiles ms)  -- includes 0 for New Level
      in return . Menu $ ms { menuFocus = min lastIdx (menuFocus + 1) }
    EventKey (Char 'S') Down _ _ ->
      let lastIdx = length (menuCustomFiles ms)
      in return . Menu $ ms { menuFocus = min lastIdx (menuFocus + 1) }

    -- Activate
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      if menuFocus == 0 then goBuilderName ms else do
        case drop (menuFocus - 1) (menuCustomFiles ms) of
          (f:_) -> startBuilderFromLevel ms ("levels/" ++ f)
          _     -> return (Menu ms)

    -- Back
    EventKey (SpecialKey KeyEsc) Down _ _ -> return . Menu $ ms { menuPage = MainMenu, menuFocus = 1 }

    -- Mouse hover
    EventMotion (mx, my) ->
      case buttonFromMouseBuilderSelect ms (mx, my) of
        Just f  -> return . Menu $ ms { menuFocus = f }
        Nothing -> return (Menu ms)

    -- Mouse click
    EventKey (MouseButton LeftButton) Down _ (mx, my) ->
      case buttonFromMouseBuilderSelect ms (mx, my) of
        Just 0 -> goBuilderName ms
        Just idx -> case drop (idx - 1) (menuCustomFiles ms) of
          (f:_) -> startBuilderFromLevel ms ("levels/" ++ f)
          _     -> return (Menu ms)
        Nothing -> return (Menu ms)

    _ -> return (Menu ms)

  BuilderName -> case e of
    -- Backspace first (both variants)
    EventKey (SpecialKey KeyBackspace) Down _ _ -> return . Menu $ ms { menuInput = if null (menuInput ms) then "" else init (menuInput ms) }
    EventKey (Char '\b') Down _ _              -> return . Menu $ ms { menuInput = if null (menuInput ms) then "" else init (menuInput ms) }
    -- Accept simple text input
    EventKey (Char c) Down _ _ ->
      let allowed = c == ' ' || c == '-' || c == '_' || c == '.' || (c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
      in if allowed then return . Menu $ ms { menuInput = menuInput ms ++ [c] } else return (Menu ms)
    -- Confirm
    EventKey (SpecialKey KeyEnter) Down _ _ -> do
      let base = if null (menuInput ms) then "untitled" else menuInput ms
          name = let ext = ".lvl" in if length base >= length ext && drop (length base - length ext) base == ext then base else base ++ ext
      startBuilder ms { menuInput = name }
    -- Cancel
    EventKey (SpecialKey KeyEsc) Down _ _ -> return . Menu $ ms { menuPage = BuilderSelect, menuFocus = 0 }
    _ -> return (Menu ms)

-- Main menu mouse focus: 3 buttons (0=Play,1=Builder,2=Custom Levels)
buttonFromMouseMain :: (Float, Float) -> Maybe Int
buttonFromMouseMain (mx, my)
  | inside 0 = Just 0
  | inside 1 = Just 1
  | inside 2 = Just 2
  | otherwise = Nothing
  where
    btnW = 460 :: Float
    btnH = 90  :: Float
    btnY :: Int -> Float
    btnY 0 = 100
    btnY 1 = 0
    btnY 2 = -100
    inside i = let cx = 0; cy = btnY i
                   dx = abs (mx - cx)
                   dy = abs (my - cy)
               in dx <= btnW/2 && dy <= btnH/2

-- Builder select mouse focus: 0 = New Level, 1.. = files
buttonFromMouseBuilderSelect :: MenuState -> (Float, Float) -> Maybe Int
buttonFromMouseBuilderSelect MenuState { menuCustomFiles } (mx, my) = go 0 (Nothing : map Just [0 .. length menuCustomFiles - 1])
  where
    btnW = 600 :: Float
    btnH = 70  :: Float
    yOf i = 160 - fromIntegral i * 80 :: Float
    inside i = let cx = 0; cy = yOf i
                   dx = abs (mx - cx)
                   dy = abs (my - cy)
               in dx <= btnW/2 && dy <= btnH/2
    go _ [] = Nothing
    go i (_:xs)
      | inside i = Just i
      | otherwise = go (i+1) xs

-- Custom levels list mouse focus
buttonFromMouseCustom :: MenuState -> (Float, Float) -> Maybe Int
buttonFromMouseCustom MenuState { menuCustomFiles } (mx, my) = go 0 menuCustomFiles
  where
    btnW = 600 :: Float
    btnH = 70  :: Float
    yOf i = 120 - fromIntegral i * 80 :: Float
    inside i = let cx = 0; cy = yOf i
                   dx = abs (mx - cx)
                   dy = abs (my - cy)
               in dx <= btnW/2 && dy <= btnH/2
    go _ [] = Nothing
    go i (_:xs)
      | inside i = Just i
      | otherwise = go (i+1) xs

-- Enter custom levels page and populate file list
goCustomLevels :: MenuState -> IO AppState
goCustomLevels ms = do
  exists <- doesDirectoryExist "levels"
  files <- if not exists then return [] else listDirectory "levels"
  let endsWith ext s = let le = length ext; ls = length s in ls >= le && drop (ls - le) s == ext
      lvls = [ f | f <- files, endsWith ".lvl" f ]
  return . Menu $ ms { menuPage = CustomLevels, menuFocus = 0, menuCustomFiles = lvls }

-- Enter builder select page
goBuilderSelect :: MenuState -> IO AppState
goBuilderSelect ms = do
  exists <- doesDirectoryExist "levels"
  files <- if not exists then return [] else listDirectory "levels"
  let endsWith ext s = let le = length ext; ls = length s in ls >= le && drop (ls - le) s == ext
      lvls = [ f | f <- files, endsWith ".lvl" f ]
  return . Menu $ ms { menuPage = BuilderSelect, menuFocus = 0, menuCustomFiles = lvls }

-- Go to builder name input
goBuilderName :: MenuState -> IO AppState
goBuilderName ms = return . Menu $ ms { menuPage = BuilderName, menuInput = "", menuFocus = 0 }

-- Start selected custom level
startCustomLevel :: MenuState -> FilePath -> IO AppState
startCustomLevel ms path = do
  tileMap <- loadTileMap
  gs <- loadLevel path (menuDebugMode ms) tileMap (menuScreenSize ms)
  return (Playing gs)

-- Start builder with an existing level loaded into the builder world
startBuilderFromLevel :: MenuState -> FilePath -> IO AppState
startBuilderFromLevel ms path = do
  tileMap <- loadTileMap
  -- reuse GameState loader to get a world grid, then convert to BuilderState
  gs <- loadLevel path (menuDebugMode ms) tileMap (menuScreenSize ms)
  let w = world gs
      bs = BuilderState
        { builderWorld = w
        , builderTileMap = tileMap
        , builderTileZoom = 1.0
        , builderScreenSize = menuScreenSize ms
        , builderDebugMode = menuDebugMode ms
        , builderBrush = Grass
        , builderCam = (0, 0)
        , builderPanning = False
        , builderLastMouse = (0, 0)
        , builderLMBHeld = False
        , builderLastPaint = Nothing
        , builderFilePath = Just path
        }
  return (Building bs)

startGame :: MenuState -> IO AppState
startGame menuState = do
  initialState <- buildInitialGameState menuState
  when (menuDebugMode menuState) $
    print (colliders (world initialState))
  return (Playing initialState)

startBuilder :: MenuState -> IO AppState
startBuilder ms@MenuState { menuDebugMode, menuScreenSize } = do
  tileMap <- loadTileMap
  let width = 31
      height = 16
      emptyRow = replicate width Air
      grid = replicate height emptyRow
      builderWorld = World { grid = grid, colliders = [], slopes = [] }
      bs = BuilderState
             { builderWorld = builderWorld
             , builderTileMap = tileMap
             , builderTileZoom = 1.0
             , builderScreenSize = menuScreenSize
             , builderDebugMode = menuDebugMode
             , builderBrush = Grass
             , builderCam = (0, 0)
             , builderPanning = False
             , builderLastMouse = (0, 0)
             , builderLMBHeld = False
             , builderLastPaint = Nothing
             , builderFilePath = Just ("levels/" ++ menuInput ms)
             }
  return (Building bs)

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

-- Level Builder input: place current brush (Grass) on left-click on non-negative tile indices
handleBuildingInput :: Event -> BuilderState -> BuilderState
-- Place grass with left click
handleBuildingInput (EventKey (MouseButton LeftButton) Down _ mousePos@(mx, my)) bs =
  let bs' = bs { builderLMBHeld = True }
  in paintAtMouse mx my bs'
handleBuildingInput (EventKey (MouseButton LeftButton) Up _ _) bs = bs { builderLMBHeld = False, builderLastPaint = Nothing }
-- Start/stop panning with right mouse button
handleBuildingInput (EventKey (MouseButton RightButton) Down _ mousePos) bs = bs { builderPanning = True, builderLastMouse = mousePos }
handleBuildingInput (EventKey (MouseButton RightButton) Up   _ _)        bs = bs { builderPanning = False }
-- Pan camera while dragging
handleBuildingInput (EventMotion (mx, my)) bs@BuilderState { builderPanning = True, builderLastMouse = (lx, ly), builderCam = (cx, cy) } =
  bs { builderCam = (cx + (mx - lx), cy + (my - ly)), builderLastMouse = (mx, my) }
handleBuildingInput (EventMotion (mx, my)) bs@BuilderState { builderLMBHeld = True } = paintAtMouse mx my (bs { builderLastMouse = (mx, my) })
handleBuildingInput (EventMotion mousePos) bs = bs { builderLastMouse = mousePos }
-- Zoom with scroll wheel
handleBuildingInput (EventKey (MouseButton WheelUp) Down _ _) bs = adjustBuilderZoom zoomStep bs
handleBuildingInput (EventKey (MouseButton WheelDown) Down _ _) bs = adjustBuilderZoom (-zoomStep) bs
handleBuildingInput _ bs = bs

adjustBuilderZoom :: Float -> BuilderState -> BuilderState
adjustBuilderZoom delta bs@BuilderState { builderTileZoom } =
  let newVal = clampTileZoom (builderTileZoom + delta)
  in bs { builderTileZoom = newVal }

-- Helper: paint current brush at mouse position if it corresponds to a new valid tile
paintAtMouse :: Float -> Float -> BuilderState -> BuilderState
paintAtMouse mx my bs@BuilderState { builderWorld = w, builderScreenSize, builderTileZoom, builderCam = (camX, camY), builderLastPaint } =
  let tilePixels = baseTilePixelSizeForScreen builderScreenSize * builderTileZoom * scaleFactor
      wx = (mx - camX) / tilePixels
      wy = (my - camY) / tilePixels
      col = floor wx
      row = floor ((-wy))
      inBounds = row >= 0 && col >= 0 && row < length (grid w) && col < length (head (grid w))
      sameAsLast = builderLastPaint == Just (col, row)
  in if inBounds && not sameAsLast
        then bs { builderWorld = setTile w (col, row) Grass, builderLastPaint = Just (col, row) }
        else bs


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
