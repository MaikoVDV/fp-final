{-# LANGUAGE OverloadedStrings #-}

module Controller.Input where

import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import System.Directory (createDirectoryIfMissing, listDirectory, doesDirectoryExist, removeFile, doesFileExist)
import System.FilePath (takeDirectory, takeFileName, dropExtension, (</>), (<.>))
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe, mapMaybe, isNothing)
import Control.Monad (when)
import Control.Exception (catch, SomeException)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import System.Random (randomRIO)

import Model.Types
import Model.TypesState
import Model.InitialState
import Model.World
import Model.Collider
import Model.Config 
import Model.Entity 
import Model.WorldMap 
import Model.WorldMapCodec 
import Model.InfiniteSegments 
import Model.InfiniteWorld 

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

-- Helper: convert BuilderState to a minimal GameState and save via LevelCodec
saveBuilderLevel :: FilePath -> BuilderState -> IO ()
saveBuilderLevel path bs = do
  let w = builderWorld bs
  -- Load minimal UI assets for type completeness (not used by saver)
  hearts <- loadHeartsUI
  counters <- loadCountersUI
  let gs = GameState
        { world = w
        , player = defaultPlayer { health = maxHealth }
        , entities = builderEntities bs
        , entityIdCounter = 0
        , tileMap = builderTileMap bs
        , animMap = builderAnimMap bs
        , uiHeartFull = let (a,_,_,_) = hearts in a
        , uiHeartHalf = let (_,b,_,_) = hearts in b
        , uiHeartEmpty = let (_,_,c,_) = hearts in c
        , uiHeartGolden = let (_,_,_,d) = hearts in d
        , uiCounters = counters
        , playerLives = 0
        , coinsCollected = 0
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
        , infiniteState = Nothing
        }
  saveLevel path gs

saveBuilderSegment :: BuilderState -> IO ()
saveBuilderSegment bs =
  case prepareSegmentSnapshot bs of
    Prelude.Left err -> putStrLn ("Unable to save infinite segment: " ++ err)
    Prelude.Right snapshot ->
      case deriveSegmentHeights (builderWorld snapshot) of
        Prelude.Left err -> putStrLn ("Unable to save infinite segment: " ++ err)
        Prelude.Right (startHeight, endHeight) -> do
          createDirectoryIfMissing True segmentsDirectory
          (levelPath, metaPath, baseName) <- allocateSegmentPaths (builderSegmentBaseName bs)
          saveBuilderLevel levelPath snapshot
          let meta = SegmentMeta
                { segmentName = baseName
                , levelPath   = levelPath
                , levelFile   = takeFileName levelPath
                , startHeight = startHeight
                , endHeight   = endHeight
                }
          BL.writeFile metaPath (encode meta)
          putStrLn ("Saved infinite segment \"" ++ baseName ++ "\" (" ++ show startHeight ++ " -> " ++ show endHeight ++ ")")

deriveSegmentHeights :: World -> Either String (Int, Int)
deriveSegmentHeights World { grid = rows }
  | null rows = Prelude.Left "level has no rows"
  | width <= 0 = Prelude.Left "level has no columns"
  | otherwise =
      case (edgeHeight 0, edgeHeight (width - 1)) of
        (Nothing, _) -> Prelude.Left "left edge must contain at least one solid tile"
        (_, Nothing) -> Prelude.Left "right edge must contain at least one solid tile"
        (Just leftH, Just rightH) -> Prelude.Right (leftH, rightH)
  where
    rowCount = length rows
    width = maximum (0 : map length rows)
    edgeHeight :: Int -> Maybe Int
    edgeHeight col = do
      rowIdx <- columnSurfaceRow rows col
      return ((rowCount - 1) - rowIdx)

columnSurfaceRow :: [[Tile]] -> Int -> Maybe Int
columnSurfaceRow rows col =
  listToMaybe
    [ y
    | (y, row) <- zip [0 ..] rows
    , col < length row
    , isSolidTile (row !! col)
    ]

isSolidTile :: Tile -> Bool
isSolidTile Air = False
isSolidTile _   = True

trimWorldForSegment :: World -> Either String (World, Int, Int)
trimWorldForSegment world@World { grid } =
  let width = if null grid then 0 else length (head grid)
      solidColumns = [ c | c <- [0 .. width - 1], columnHasSolid c ]
  in case solidColumns of
       [] -> Prelude.Left "level needs at least one solid tile touching the edges"
       cols ->
         let startCol = head cols
             endCol = last cols
             slice row = take (endCol - startCol + 1) (drop startCol row)
             trimmedGrid = map slice grid
             trimmedWorld = world { grid = trimmedGrid
                                  , colliders = generateCollidersForWorld trimmedGrid
                                  }
         in Prelude.Right (trimmedWorld, startCol, endCol - startCol + 1)
  where
    columnHasSolid :: Int -> Bool
    columnHasSolid idx =
      any (\row -> idx < length row && isSolidTile (row !! idx)) grid

shiftEntitiesAfterTrim :: Int -> Int -> [Entity] -> [Entity]
shiftEntitiesAfterTrim leftTrim newWidth =
  let dx = fromIntegral leftTrim
      maxX = fromIntegral newWidth
  in mapMaybe (shiftEntityIntoRange dx maxX)

shiftEntityIntoRange :: Float -> Float -> Entity -> Maybe Entity
shiftEntityIntoRange dx maxX entity =
  case entity of
    EGoomba eid g@Goomba { goombaPos = (x, y) } ->
      let x' = x - dx in
      if withinBounds x' maxX
        then Just (EGoomba eid (g { goombaPos = (x', y) }))
        else Nothing
    EKoopa eid k@Koopa { koopaPos = (x, y) } ->
      let x' = x - dx in
      if withinBounds x' maxX
        then Just (EKoopa eid (k { koopaPos = (x', y) }))
        else Nothing
    EPowerup eid p@Powerup { powerupPos = (x, y) } ->
      let x' = x - dx in
      if withinBounds x' maxX
        then Just (EPowerup eid (p { powerupPos = (x', y) }))
        else Nothing
    ECoin eid c@Coin { coinPos = (x, y) } ->
      let x' = x - dx in
      if withinBounds x' maxX
        then Just (ECoin eid (c { coinPos = (x', y) }))
        else Nothing

withinBounds :: Float -> Float -> Bool
withinBounds x maxX = x >= 0 && x < maxX

builderSegmentBaseName :: BuilderState -> String
builderSegmentBaseName bs =
  case builderFilePath bs >>= nonEmpty . dropExtension . takeFileName of
    Just name -> name
    Nothing   -> "segment"
  where
    nonEmpty "" = Nothing
    nonEmpty s  = Just s

allocateSegmentPaths :: String -> IO (FilePath, FilePath, String)
allocateSegmentPaths baseName = go 0
  where
    go :: Int -> IO (FilePath, FilePath, String)
    go n = do
      let suffix = if n == 0 then baseName else baseName ++ "-" ++ show n
          levelPath = segmentsDirectory </> (suffix <.> "lvl")
          metaPath  = segmentsDirectory </> (suffix ++ segmentMetaSuffix)
      levelExists <- doesFileExist levelPath
      metaExists  <- doesFileExist metaPath
      if not levelExists && not metaExists
        then return (levelPath, metaPath, suffix)
        else go (n + 1)

prepareSegmentSnapshot :: BuilderState -> Either String BuilderState
prepareSegmentSnapshot bs = do
  (trimmedWorld, leftTrim, newWidth) <- trimWorldForSegment (builderWorld bs)
  let shiftedEntities = shiftEntitiesAfterTrim leftTrim newWidth (builderEntities bs)
  return bs { builderWorld = trimmedWorld, builderEntities = shiftedEntities }

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

-- Main menu mouse focus: 4 buttons (0=Play,1=Builder,2=Custom Levels,3=Infinite Mode)
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
    Just (SelEnemy EnemyKoopa)  -> bs { builderEnemySel = EnemyKoopa,  builderLMBHeld = False, builderLastPaint = Nothing }
    Just (SelEnemy EnemyCoin)   -> bs { builderEnemySel = EnemyCoin,   builderLMBHeld = False, builderLastPaint = Nothing }
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

-- Paints the selected tile at mouse position, if that position is valid to be drawn on
paintAtMouse :: Float -> Float -> BuilderState -> BuilderState
paintAtMouse mx my bs@BuilderState { builderWorld = w, builderScreenSize, builderTileZoom, builderCam = (camX, camY), builderLastPaint, builderBrush, builderBrushMode, builderPaletteTab, builderEntities, builderEnemySel } =
  let tilePixels = baseTilePixelSizeForScreen builderScreenSize * builderTileZoom * scaleFactor
      wx = (mx - camX) / tilePixels
      wy = (my - camY) / tilePixels
      col = floor wx
      row = floor (-wy)
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
                       EKoopa  _ Koopa  { koopaPos  = p } -> cellOf p == (cx, cy)
                       ECoin   _ Coin   { coinPos   = p } -> cellOf p == (cx, cy)
                       _ -> False
                     existsHere = any isAtCell builderEntities
                     ents' = case builderEnemySel of
                               EnemyEraser -> filter (not . isAtCell) builderEntities
                               EnemyGoomba -> if existsHere
                                                then filter (not . isAtCell) builderEntities
                                                else EGoomba 0 defaultGoomba { goombaPos=(cxWorld, cyWorld) } : builderEntities
                               EnemyKoopa  -> if existsHere
                                                then filter (not . isAtCell) builderEntities
                                                else EKoopa 0 defaultKoopa { koopaPos=(cxWorld, cyWorld) } : builderEntities
                               EnemyCoin   -> if existsHere
                                                then filter (not . isAtCell) builderEntities
                                                else ECoin  0 defaultCoin  { coinPos=(cxWorld, cyWorld) } : builderEntities
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

-- Two-column grid beneath a tabs header
data PaletteSel = SelTile Tile | SelTool BrushMode | SelTab PaletteTab | SelEnemy EnemySel

-- Palette hit test for both tabs
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
              PEnemyKoopa    -> Just (SelEnemy EnemyKoopa)
              PEnemyCoin     -> Just (SelEnemy EnemyCoin)
              PEnemyEraser   -> Just (SelEnemy EnemyEraser)
        | otherwise -> Nothing

-- Palette tiles in desired order (excluding Air)
data SpecialBrush = GrassColumn | Eraser

data PaletteItem = PTile Tile | PSpecial SpecialBrush | PEnemyGoomba | PEnemyKoopa | PEnemyCoin | PEnemyEraser

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
  , PEnemyKoopa
  , PEnemyCoin
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
