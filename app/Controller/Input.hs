module Controller.Input where

import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist, removeFile)
import System.FilePath (takeDirectory)
import Data.List (isPrefixOf)
import Control.Monad (when)
import Control.Exception (catch, SomeException)

import Model.Types
import LevelCodec
import MathUtils
import Model.InitialState
import Model.Config
import Model.World
import Assets
import qualified Data.Map as Map
import Model.Entity (defaultPlayer, defaultGoomba)
import Model.WorldMap (exampleWorldMap, NodeId(..), adjacentDirected, Edge(..), nodeById, LevelRef(..), MapNode(..))
import LevelCodec (loadLevel)
import Model.WorldMapCodec (loadWorldMapFile)
import System.Directory (doesFileExist)

input :: Event -> AppState -> IO AppState
-- Press 's' in build mode to save the current level
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

-- Helper: convert BuilderState to a minimal GameState and save via LevelCodec
saveBuilderLevel :: FilePath -> BuilderState -> IO ()
saveBuilderLevel path bs = do
  let w = builderWorld bs
      gs = GameState
        { world = w
        , player = defaultPlayer
        , entities = builderEntities bs
        , entityIdCounter = 0
        , tileMap = builderTileMap bs
        , animMap = builderAnimMap bs
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
        , currentMapState = Nothing
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
        0 -> startWorldMap ms
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
        Just 0 -> startWorldMap ms
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
    -- Navigate grid with WASD
    EventKey (Char 'w') Down _ _ -> return . Menu $ ms { menuFocus = moveFocusBuilderSelect ms menuFocus 'w' }
    EventKey (Char 'W') Down _ _ -> return . Menu $ ms { menuFocus = moveFocusBuilderSelect ms menuFocus 'w' }
    EventKey (Char 's') Down _ _ -> return . Menu $ ms { menuFocus = moveFocusBuilderSelect ms menuFocus 's' }
    EventKey (Char 'S') Down _ _ -> return . Menu $ ms { menuFocus = moveFocusBuilderSelect ms menuFocus 's' }
    EventKey (Char 'a') Down _ _ -> return . Menu $ ms { menuFocus = moveFocusBuilderSelect ms menuFocus 'a' }
    EventKey (Char 'A') Down _ _ -> return . Menu $ ms { menuFocus = moveFocusBuilderSelect ms menuFocus 'a' }
    EventKey (Char 'd') Down _ _ -> return . Menu $ ms { menuFocus = moveFocusBuilderSelect ms menuFocus 'd' }
    EventKey (Char 'D') Down _ _ -> return . Menu $ ms { menuFocus = moveFocusBuilderSelect ms menuFocus 'd' }

    -- Activate
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      case focusToRC menuFocus of
        (0, 0) -> goBuilderName ms
        (r, 0) | r >= 1 -> case drop (r - 1) (menuCustomFiles ms) of
                              (f:_) -> startBuilderFromLevel ms ("levels/" ++ f)
                              _     -> return (Menu ms)
        (r, 1) | r >= 1 -> do
          let files = menuCustomFiles ms
          case drop (r - 1) files of
            (f:_) -> do
              let path = "levels/" ++ f
                  handler :: SomeException -> IO ()
                  handler _ = return ()
              catch (removeFile path) handler
              refreshBuilderSelect ms
            _ -> return (Menu ms)
        _ -> return (Menu ms)

    -- Back
    EventKey (SpecialKey KeyEsc) Down _ _ -> return . Menu $ ms { menuPage = MainMenu, menuFocus = 1 }

    -- Mouse hover
    EventMotion (mx, my) ->
      case focusFromMouseBuilderSelect ms (mx, my) of
        Just f  -> return . Menu $ ms { menuFocus = f }
        Nothing -> return (Menu ms)

    -- Mouse click
    EventKey (MouseButton LeftButton) Down _ (mx, my) -> do
      case focusFromMouseBuilderSelect ms (mx, my) of
        Just f -> case focusToRC f of
          (0, 0) -> goBuilderName ms
          (r, 0) | r >= 1 -> case drop (r - 1) (menuCustomFiles ms) of
                                (ff:_) -> startBuilderFromLevel ms ("levels/" ++ ff)
                                _      -> return (Menu ms)
          (r, 1) | r >= 1 -> do
            let files = menuCustomFiles ms
            case drop (r - 1) files of
              (ff:_) -> do
                let path = "levels/" ++ ff
                    handler :: SomeException -> IO ()
                    handler _ = return ()
                catch (removeFile path) handler
                refreshBuilderSelect ms
              _ -> return (Menu ms)
          _ -> return (Menu ms)
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
-- Convert a focus Int to (row, col)
focusToRC :: Int -> (Int, Int)
focusToRC f = (f `div` 2, f `mod` 2)

-- Normalize focus for BuilderSelect page so it targets an existing button
normalizeFocusBuilderSelect :: MenuState -> Int -> Int
normalizeFocusBuilderSelect MenuState { menuCustomFiles } f =
  let rows = 1 + length menuCustomFiles
      (r0, c0) = focusToRC f
      r = max 0 (min (rows - 1) r0)
      maxC = if r == 0 then 0 else 1
      c = max 0 (min maxC c0)
  in r * 2 + c

-- Move focus per WASD within the grid
moveFocusBuilderSelect :: MenuState -> Int -> Char -> Int
moveFocusBuilderSelect ms f dir =
  let (r, c) = focusToRC (normalizeFocusBuilderSelect ms f)
      rows = 1 + length (menuCustomFiles ms)
      up    = max 0 (r - 1)
      down  = min (rows - 1) (r + 1)
      leftC = if r == 0 then 0 else max 0 (c - 1)
      rightC= if r == 0 then 0 else min 1 (c + 1)
  in normalizeFocusBuilderSelect ms $ case dir of
      'w' -> up * 2 + min c (if up == 0 then 0 else 1)
      's' -> down * 2 + min c (if down == 0 then 0 else 1)
      'a' -> r * 2 + leftC
      'd' -> r * 2 + rightC
      _   -> r * 2 + c

-- Mouse to focus mapping for BuilderSelect (includes New Level and Delete buttons)
focusFromMouseBuilderSelect :: MenuState -> (Float, Float) -> Maybe Int
focusFromMouseBuilderSelect MenuState { menuCustomFiles } (mx, my) =
  let btnW = 600 :: Float
      btnH = 70  :: Float
      delW = 160 :: Float
      delH = btnH
      delX = btnW/2 + 40 + delW/2
      yOf i = 160 - fromIntegral i * 80 :: Float
      insideRect cx cy w h = let dx = abs (mx - cx); dy = abs (my - cy) in dx <= w/2 && dy <= h/2
      -- check New Level first (row 0, col 0)
      checkRow0 = if insideRect 0 (yOf 0) btnW btnH then Just 0 else Nothing
      -- check file rows
      checkRow i [] = Nothing
      checkRow i (_:xs) =
        let y = yOf i
            leftHit  = insideRect 0   y btnW btnH
            rightHit = insideRect delX y delW delH
        in if leftHit then Just (i*2)
           else if rightHit then Just (i*2 + 1)
           else checkRow (i+1) xs
  in case checkRow0 of
      Just f -> Just f
      Nothing -> checkRow 1 menuCustomFiles

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
      f0 = 0 -- row 0, col 0
  return . Menu $ ms { menuPage = BuilderSelect, menuFocus = f0, menuCustomFiles = lvls }

-- Refresh file list in BuilderSelect after a deletion
refreshBuilderSelect :: MenuState -> IO AppState
refreshBuilderSelect ms = do
  exists <- doesDirectoryExist "levels"
  files <- if not exists then return [] else listDirectory "levels"
  let endsWith ext s = let le = length ext; ls = length s in ls >= le && drop (ls - le) s == ext
      lvls = [ f | f <- files, endsWith ".lvl" f ]
      newFocus = normalizeFocusBuilderSelect (ms { menuCustomFiles = lvls }) (menuFocus ms)
  return . Menu $ ms { menuPage = BuilderSelect, menuFocus = newFocus, menuCustomFiles = lvls }

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
  animMap <- loadAnimMap
  -- reuse GameState loader to get a world grid, then convert to BuilderState
  gs <- loadLevel path (menuDebugMode ms) tileMap (menuScreenSize ms)
  let w = world gs
      bs = BuilderState
        { builderWorld = w
        , builderTileMap = tileMap
        , builderAnimMap = animMap
        , builderTileZoom = 1.0
        , builderScreenSize = menuScreenSize ms
        , builderDebugMode = menuDebugMode ms
        , builderBrush = Grass
        , builderBrushMode = BrushNormal
        , builderDirty = False
        , builderConfirmLeave = False
        , builderPaletteTab = TabBlocks
        , builderEnemySel = EnemyGoomba
        , builderEntities = entities gs
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

-- Enter the world map screen (press Enter there to start the default level)
startWorldMap :: MenuState -> IO AppState
startWorldMap ms = do
  let worldPath = "worlds/default.json"
  hasFile <- doesFileExist worldPath
  if hasFile
    then do
      e <- loadWorldMapFile worldPath
      case e of
        Prelude.Left err -> do
          putStrLn ("Failed to load world file: " ++ err)
          return (Menu ms)
        Prelude.Right wm -> do
          let initial = MapState { wmWorldMap = wm
                                  , wmCursor = NodeId 0
                                  , wmMenuState = ms
                                  , wmAlong = Nothing
                                  , wmSpeed = 240
                                  , wmFilePath = worldPath
                                  }
          return (WorldMapScreen initial)
    else do
      putStrLn ("World file not found: " ++ worldPath)
      return (Menu ms)

-- Basic world map input: Esc -> back to menu; Enter -> start default game
handleWorldMapInput :: Event -> MapState -> IO AppState
handleWorldMapInput e ms@MapState { wmWorldMap = wm, wmCursor = cur, wmAlong } = case e of
  EventKey (SpecialKey KeyEsc) Down _ _   -> return (Menu (wmMenuState ms))
  EventKey (SpecialKey KeyEnter) Down _ _ -> startCurrentNodeLevel ms
  EventKey (Char 'w') Down _ _ | wmAlong == Nothing -> return (tryMove (0,1))
  EventKey (Char 'W') Down _ _ | wmAlong == Nothing -> return (tryMove (0,1))
  EventKey (Char 's') Down _ _ | wmAlong == Nothing -> return (tryMove (0,-1))
  EventKey (Char 'S') Down _ _ | wmAlong == Nothing -> return (tryMove (0,-1))
  EventKey (Char 'a') Down _ _ | wmAlong == Nothing -> return (tryMove (-1,0))
  EventKey (Char 'A') Down _ _ | wmAlong == Nothing -> return (tryMove (-1,0))
  EventKey (Char 'd') Down _ _ | wmAlong == Nothing -> return (tryMove (1,0))
  EventKey (Char 'D') Down _ _ | wmAlong == Nothing -> return (tryMove (1,0))
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
                    (e, nb, pts, _) = bestO
                in WorldMapScreen (ms { wmAlong = Just (edgeId e, pts, nb, 0) })

    dot (x1,y1) (x2,y2) = x1*x2 + y1*y2

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

startBuilder :: MenuState -> IO AppState
startBuilder ms@MenuState { menuDebugMode, menuScreenSize } = do
  tileMap <- loadTileMap
  animMap <- loadAnimMap
  let width = 31
      height = 16
      emptyRow = replicate width Air
      grid = replicate height emptyRow
      builderWorld = World { grid = grid, colliders = [], slopes = [] }
      bs = BuilderState
              { builderWorld = builderWorld
             , builderTileMap = tileMap
             , builderAnimMap = animMap
             , builderTileZoom = 1.0
             , builderScreenSize = menuScreenSize
             , builderDebugMode = menuDebugMode
             , builderBrush = Grass
             , builderBrushMode = BrushNormal
             , builderDirty = False
             , builderConfirmLeave = False
             , builderPaletteTab = TabBlocks
             , builderEnemySel = EnemyGoomba
             , builderEntities = []
             , builderCam = (0, 0)
             , builderPanning = False
             , builderLastMouse = (0, 0)
             , builderLMBHeld = False
             , builderLastPaint = Nothing
              , builderFilePath = Just (((if menuDebugMode then "built-in-levels/" else "levels/") ) ++ menuInput ms)
              }
  return (Building bs)

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

-- Level Builder input: place current brush (Grass) on left-click on non-negative tile indices
handleBuildingInput :: Event -> BuilderState -> BuilderState
-- Left click: if inside palette, select brush; otherwise start painting
handleBuildingInput (EventKey (MouseButton LeftButton) Down _ (mx, my)) bs@BuilderState { builderScreenSize, builderPaletteTab } =
  case paletteHit builderScreenSize builderPaletteTab (mx, my) of
    Just (SelTab tab)   -> bs { builderPaletteTab = tab, builderLMBHeld = False }
    Just (SelTile tile) -> bs { builderBrush = tile, builderBrushMode = BrushNormal, builderLMBHeld = False, builderLastPaint = Nothing }
    Just (SelTool mode) -> bs { builderBrushMode = mode, builderLMBHeld = False, builderLastPaint = Nothing }
    Just (SelEnemy EnemyGoomba) -> bs { builderEnemySel = EnemyGoomba, builderLMBHeld = False, builderLastPaint = Nothing }
    Just (SelEnemy EnemyEraser) -> bs { builderEnemySel = EnemyEraser, builderLMBHeld = False, builderLastPaint = Nothing }
    Nothing   -> let bs' = bs { builderLMBHeld = True }
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
paintAtMouse mx my bs@BuilderState { builderWorld = w, builderScreenSize, builderTileZoom, builderCam = (camX, camY), builderLastPaint, builderBrush, builderBrushMode, builderPaletteTab, builderEntities, builderEnemySel } =
  let tilePixels = baseTilePixelSizeForScreen builderScreenSize * builderTileZoom * scaleFactor
      wx = (mx - camX) / tilePixels
      wy = (my - camY) / tilePixels
      col = floor wx
      row = floor ((-wy))
      inBounds = row >= 0 && col >= 0 && row < length (grid w) && (null (grid w) || col < length (head (grid w)))
      (wExpanded, (cx, cy)) = if inBounds then (w, (col, row)) else ensureInBounds w (col, row)
      sameAsLast = builderLastPaint == Just (cx, cy)
  in if not sameAsLast
        then case builderPaletteTab of
               TabEnemies ->
                 let cxWorld = fromIntegral cx + 0.5
                     cyWorld = negate (fromIntegral cy) - 0.5
                     cellOf (x', y') = (floor x', floor (-y'))
                     isAtCell e = case e of
                       EGoomba _ Goomba { goombaPos = p } -> cellOf p == (cx, cy)
                       _ -> False
                     existsHere = any isAtCell builderEntities
                     ents' = case builderEnemySel of
                               EnemyEraser -> filter (not . isAtCell) builderEntities
                               EnemyGoomba -> if existsHere
                                                then filter (not . isAtCell) builderEntities
                                                else (EGoomba 0 defaultGoomba { goombaPos=(cxWorld, cyWorld) } : builderEntities)
                 in bs { builderWorld = wExpanded
                       , builderEntities = ents'
                       , builderLastPaint = Just (cx, cy)
                       , builderDirty = True }
               TabBlocks -> case builderBrushMode of
                 BrushNormal -> bs { builderWorld = setTile wExpanded (cx, cy) builderBrush
                                    , builderLastPaint = Just (cx, cy)
                                    , builderDirty = True }
                 BrushGrassColumn ->
                   let w1 = setTile wExpanded (cx, cy) Grass
                       h = length (grid w1)
                       fillDown world y0
                         | y0 >= h = world
                         | otherwise =
                             case getTile world (cx, y0) of
                               Air -> fillDown (setTile world (cx, y0) Earth) (y0+1)
                               _   -> world
                       wFilled = fillDown w1 (cy + 1)
                   in bs { builderWorld = wFilled, builderLastPaint = Just (cx, cy), builderDirty = True }
                 BrushEraser -> bs { builderWorld = setTile wExpanded (cx, cy) Air
                                     , builderLastPaint = Just (cx, cy)
                                     , builderDirty = True }
        else bs

-- Hit test for leave confirmation popup buttons: True = Yes, False = No
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

-- Palette hit test: with tabs and enemies
-- Two-column grid beneath a tabs header
data PaletteSel = SelTile Tile | SelTool BrushMode | SelTab PaletteTab | SelEnemy EnemySel

paletteHit :: (Int, Int) -> PaletteTab -> (Float, Float) -> Maybe PaletteSel
paletteHit (screenW, screenH) currentTab (mx, my) =
  let sw = fromIntegral screenW :: Float
      sh = fromIntegral screenH :: Float
      panelW = sw / 6
      leftX  =  sw/2 - panelW
      rightX =  sw/2
      topY   =  sh/2
      bottomY = -sh/2
      insidePanel = mx >= leftX && mx <= rightX && my >= bottomY && my <= topY
      tabH = 60 :: Float
      inTabs = insidePanel && my <= topY && my >= topY - tabH
      inGrid = insidePanel && my < topY - tabH
      tabSel | not inTabs = Nothing
             | mx < leftX + panelW/2 = Just (SelTab TabBlocks)
             | otherwise              = Just (SelTab TabEnemies)
      items = case currentTab of
        TabBlocks  -> paletteItemsBlocks
        TabEnemies -> paletteItemsEnemies
      n = length items
      cols = 2 :: Int
      rows = (n + cols - 1) `div` cols
      cellW = panelW / fromIntegral cols
      gridH = sh - tabH
      slotH = gridH / fromIntegral (max 1 rows)
      colIdx | mx < leftX + cellW = 0
             | otherwise          = 1
      rowIdx | slotH <= 0 = -1
             | otherwise  = floor (((topY - tabH) - my) / slotH)
      idx = colIdx * rows + rowIdx
  in case () of
      _ | inTabs -> tabSel
        | inGrid && rowIdx >= 0 && rowIdx < rows && idx >= 0 && idx < n ->
            case items !! idx of
              PTile t        -> Just (SelTile t)
              PSpecial sb    -> Just (SelTool (specialToMode sb))
              PEnemyGoomba   -> Just (SelEnemy EnemyGoomba)
              PEnemyEraser   -> Just (SelEnemy EnemyEraser)
        | otherwise -> Nothing

-- Palette tiles in desired order (excluding Air)
data SpecialBrush = GrassColumn | Eraser

data PaletteItem = PTile Tile | PSpecial SpecialBrush | PEnemyGoomba | PEnemyEraser

specialToMode :: SpecialBrush -> BrushMode
specialToMode GrassColumn = BrushGrassColumn
specialToMode Eraser      = BrushEraser

paletteItemsBlocks :: [PaletteItem]
paletteItemsBlocks =
  [ PSpecial GrassColumn
  , PSpecial Eraser
  , PTile Grass
  , PTile Earth
  , PTile Crate
  , PTile MetalBox
  , PTile QuestionBlockFull
  , PTile QuestionBlockEmpty
  , PTile Flag
  , PTile Spikes
  ]

paletteItemsEnemies :: [PaletteItem]
paletteItemsEnemies =
  [ PEnemyEraser
  , PEnemyGoomba
  ]


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

-- Pause menu hit test: True = Resume, False = Main Menu
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
