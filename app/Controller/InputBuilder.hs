module Controller.InputBuilder where

import System.Directory
import System.FilePath
import Graphics.Gloss.Interface.IO.Game
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import Data.Aeson

import Model.TypesState
import Model.Types
import Model.Config
import Model.World
import Model.InitialState
import Model.Entity
import Model.InfiniteSegments
import Model.Collider

import LevelCodec
import Assets
import MathUtils

-- Two-column grid beneath a tabs header
data PaletteSel = SelTile Tile | SelTool BrushMode | SelTab PaletteTab | SelEnemy EnemySel
data SpecialBrush = GrassColumn | Eraser
data PaletteItem = PTile Tile | PSpecial SpecialBrush | PGoomba | PKoopa | PEnemyCoin | PEnemyEraser

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

-- Which entities should appear in the palette in the builder
paletteItemsEntities :: [PaletteItem]
paletteItemsEntities =
  [ PEnemyEraser
  , PGoomba
  , PKoopa
  , PEnemyCoin
  ]

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
        TabEnemies -> paletteItemsEntities
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
              PTile t      -> Just (SelTile t)
              PSpecial sb  -> Just (SelTool (specialToMode sb))
              PGoomba      -> Just (SelEnemy EnemyGoomba)
              PKoopa       -> Just (SelEnemy EnemyKoopa)
              PEnemyCoin   -> Just (SelEnemy EnemyCoin)
              PEnemyEraser -> Just (SelEnemy EnemyEraser)
        | otherwise -> Nothing



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

prepareSegmentSnapshot :: BuilderState -> Either String BuilderState
prepareSegmentSnapshot bs = do
  (trimmedWorld, leftTrim, newWidth) <- trimWorldForSegment (builderWorld bs)
  let shiftedEntities = shiftEntitiesAfterTrim leftTrim newWidth (builderEntities bs)
  return bs { builderWorld = trimmedWorld, builderEntities = shiftedEntities }

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

builderSegmentBaseName :: BuilderState -> String
builderSegmentBaseName bs =
  case builderFilePath bs >>= nonEmpty . dropExtension . takeFileName of
    Just name -> name
    Nothing   -> "segment"
  where
    nonEmpty "" = Nothing
    nonEmpty s  = Just s

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