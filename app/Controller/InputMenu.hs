module Controller.InputMenu where
import Graphics.Gloss.Interface.IO.Game
import Model.TypesState
import Control.Exception
import System.Exit
import System.Directory
import Model.WorldMapCodec
import Model.WorldMap
import Model.Types
import System.Random
import Model.InfiniteSegments
import Assets
import LevelCodec
import Model.InfiniteWorld

handleMenuInput :: Event -> MenuState -> IO AppState
handleMenuInput e ms@MenuState { menuFocus, menuPage } = case menuPage of
  MainMenu -> case e of
    -- Quit from main menu on ESC
    EventKey (SpecialKey KeyEsc) Down _ _ -> exitSuccess >> return (Menu ms)
    -- Keyboard focus navigation (W/S)
    EventKey (Char 'w') Down _ _ -> return . Menu $ ms { menuFocus = max 0 (menuFocus - 1) }
    EventKey (Char 'W') Down _ _ -> return . Menu $ ms { menuFocus = max 0 (menuFocus - 1) }
    EventKey (Char 's') Down _ _ -> return . Menu $ ms { menuFocus = min 3 (menuFocus + 1) }
    EventKey (Char 'S') Down _ _ -> return . Menu $ ms { menuFocus = min 3 (menuFocus + 1) }

    -- Activate focused button
    EventKey (SpecialKey KeyEnter) Down _ _ ->
      case menuFocus of
        0 -> startWorldMap ms
        1 -> goBuilderSelect ms
        2 -> goCustomLevels ms
        3 -> startInfiniteMode ms
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
        Just 3 -> startInfiniteMode ms
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
          -- Level select buttons
          (r, 0) | r >= 1 -> case drop (r - 1) (menuCustomFiles ms) of
                                (ff:_) -> startBuilderFromLevel ms ("levels/" ++ ff)
                                _      -> return (Menu ms)
          -- Delete buttons
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

-- Convert a focus Int to (row, col)
focusToRC :: Int -> (Int, Int)
focusToRC f = (f `div` 2, f `mod` 2)

-- Main menu mouse focus: 4 buttons (Play, Builder, Custom levels, Infinite mode)
buttonFromMouseMain :: (Float, Float) -> Maybe Int
buttonFromMouseMain (mx, my)
  | inside 0 = Just 0
  | inside 1 = Just 1
  | inside 2 = Just 2
  | inside 3 = Just 3
  | otherwise = Nothing
  where
    btnW = 460 :: Float
    btnH = 90  :: Float
    btnY :: Int -> Float
    btnY 0 = 150
    btnY 1 = 50
    btnY 2 = -50
    btnY 3 = -150
    btnY _ = undefined -- Non-positioned buttons throw an error
    inside i = let cx = 0; cy = btnY i
                   dx = abs (mx - cx)
                   dy = abs (my - cy)
               in dx <= btnW/2 && dy <= btnH/2

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

-- Opens the Builder screen from the MainMenu
startBuilder :: MenuState -> IO AppState
startBuilder ms@MenuState { menuDebugMode, menuScreenSize } = do
  tileMap <- loadTileMap
  animMap <- loadAnimMap
  let width = 31
      height = 16
      emptyRow = replicate width Air
      grid = replicate height emptyRow
      builderWorld = World { grid = grid, colliders = [] }
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
              , builderFilePath = Just ((if menuDebugMode then "built-in-levels/" else "levels/") ++ menuInput ms)
              }
  return (Building bs)

-- Enter builder select page
goBuilderSelect :: MenuState -> IO AppState
goBuilderSelect ms = do
  exists <- doesDirectoryExist "levels"
  files <- if not exists then return [] else listDirectory "levels"
  let endsWith ext s = let le = length ext; ls = length s in ls >= le && drop (ls - le) s == ext
      lvls = [ f | f <- files, endsWith ".lvl" f ]
      f0 = 0 -- row 0, col 0
  return . Menu $ ms { menuPage = BuilderSelect, menuFocus = f0, menuCustomFiles = lvls }


-- Enter custom levels page and populate file list
goCustomLevels :: MenuState -> IO AppState
goCustomLevels ms = do
  exists <- doesDirectoryExist "levels"
  files <- if not exists then return [] else listDirectory "levels"
  let endsWith ext s = let le = length ext; ls = length s in ls >= le && drop (ls - le) s == ext
      lvls = [ f | f <- files, endsWith ".lvl" f ]
  return . Menu $ ms { menuPage = CustomLevels, menuFocus = 0, menuCustomFiles = lvls }

-- Starts the game in infinite mode, where segments are added at random, so not level-based
startInfiniteMode :: MenuState -> IO AppState
startInfiniteMode ms@MenuState { menuDebugMode, menuScreenSize } = do
  metas <- loadSegmentMetas
  case metas of
    [] -> do
      putStrLn "No infinite-mode segments found. Save segments from the builder with 'p' in debug mode first."
      return (Menu ms)
    _ -> do
      idx <- randomRIO (0, length metas - 1)
      let firstMeta = metas !! idx
      tileMap <- loadTileMap
      gs <- loadLevel (levelPath firstMeta) menuDebugMode tileMap menuScreenSize
      let width = case grid (world gs) of
                    []    -> 0
                    (r:_) -> length r
      if width <= 0
        then do
          putStrLn ("Segment \"" ++ segmentName firstMeta ++ "\" is empty. Choose a different segment.")
          return (Menu ms)
        else do
          let infState = InfiniteRunState
                { infSegments = [ActiveSegment firstMeta 0 width]
                , infSegmentPool = metas
                , infSegmentsAhead = segmentsAheadDefault
                }
              seeded = gs
                { menuState = ms
                , currentMapState = Nothing
                , nextState = NPlaying
                , infiniteState = Just infState
                }
          gsReady <- ensureInfiniteSegments seeded
          return (Playing gsReady)

-- Start selected custom level
startCustomLevel :: MenuState -> FilePath -> IO AppState
startCustomLevel ms path = do
  tileMap <- loadTileMap
  gs <- loadLevel path (menuDebugMode ms) tileMap (menuScreenSize ms)
  return (Playing gs)


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

-- Normalize focus for BuilderSelect page so it targets an existing button
normalizeFocusBuilderSelect :: MenuState -> Int -> Int
normalizeFocusBuilderSelect MenuState { menuCustomFiles } f =
  let rows = 1 + length menuCustomFiles
      (r0, c0) = focusToRC f
      r = max 0 (min (rows - 1) r0)
      maxC = if r == 0 then 0 else 1
      c = max 0 (min maxC c0)
  in r * 2 + c

-- Go to builder name input
goBuilderName :: MenuState -> IO AppState
goBuilderName ms = return . Menu $ ms { menuPage = BuilderName, menuInput = "", menuFocus = 0 }

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


-- Refresh file list in BuilderSelect after a deletion
refreshBuilderSelect :: MenuState -> IO AppState
refreshBuilderSelect ms = do
  exists <- doesDirectoryExist "levels"
  files <- if not exists then return [] else listDirectory "levels"
  let endsWith ext s = let le = length ext; ls = length s in ls >= le && drop (ls - le) s == ext
      lvls = [ f | f <- files, endsWith ".lvl" f ]
      newFocus = normalizeFocusBuilderSelect (ms { menuCustomFiles = lvls }) (menuFocus ms)
  return . Menu $ ms { menuPage = BuilderSelect, menuFocus = newFocus, menuCustomFiles = lvls }

-- Mouse to focus mapping for BuilderSelect (includes New Level and Delete buttons)
focusFromMouseBuilderSelect :: MenuState -> (Float, Float) -> Maybe Int
focusFromMouseBuilderSelect MenuState { menuCustomFiles } (mx, my) =
  let btnW = 600 :: Float
      btnH = 70  :: Float
      delW = 160 :: Float
      delH = btnH
      delX = btnW/2 + 40 + delW/2
      yOf :: Int -> Float
      yOf i = 160 - fromIntegral i * 80
      insideRect cx cy w h = let dx = abs (mx - cx); dy = abs (my - cy) in dx <= w/2 && dy <= h/2
      -- check New Level first (row 0, col 0)
      checkRow0 = if insideRect 0 (yOf 0) btnW btnH then Just 0 else Nothing
      -- check file rows
      checkRow _ [] = Nothing
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