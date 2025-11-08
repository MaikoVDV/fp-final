module View.Builder where

import Graphics.Gloss

import Model.Types
import Model.InitialState
import Model.Config
import qualified Data.Map as Map
import View.Helpers
import View.Entity

viewBuilder :: BuilderState -> Picture
viewBuilder bs@BuilderState { builderWorld, builderTileMap, builderAnimMap, builderEntities, builderTileZoom, builderScreenSize, builderDebugMode, builderCam = (camX, camY), builderConfirmLeave } =
  let tilePixels = baseTilePixelSizeForScreen builderScreenSize * builderTileZoom * scaleFactor
      worldPic   = renderBuilderWorld tilePixels builderWorld builderTileMap builderDebugMode
      previewPic = renderBuilderPreview tilePixels bs
      entsPic    = renderBuilderEntities tilePixels builderAnimMap builderEntities
      palettePic = renderBuilderPalette bs
      confirmPic = if builderConfirmLeave then renderLeaveConfirm builderScreenSize else blank
  in Pictures [ translate camX camY (Pictures [worldPic, entsPic, previewPic])
              , palettePic
              , confirmPic
              ]

renderBuilderPalette :: BuilderState -> Picture
renderBuilderPalette BuilderState { builderScreenSize = (screenW, screenH), builderTileMap = tileMap, builderAnimMap = animMap, builderBrush, builderBrushMode, builderPaletteTab, builderEnemySel } =
  let sw = fromIntegral screenW :: Float
      sh = fromIntegral screenH :: Float
      panelW = sw / 6
      leftX  =  sw/2 - panelW
      rightX =  sw/2
      topY   =  sh/2
      bottomY = -sh/2
      -- panel background
      panelRect = polygon [(leftX,bottomY),(rightX,bottomY),(rightX,topY),(leftX,topY)]
      panelBg   = color (makeColor 0.9 0.9 0.9 0.5) panelRect
      tabH = 60 :: Float
      tabBg sel cx = let w = panelW/2; h = tabH in translate cx (topY - h/2) $ Pictures
             [ color (makeColor 1 1 1 (if sel then 0.35 else 0.2)) (polygon [(-w/2,-h/2),(w/2,-h/2),(w/2,h/2),(-w/2,h/2)])
             , color black (lineLoop [(-w/2,-h/2),(w/2,-h/2),(w/2,h/2),(-w/2,h/2)])
             ]
      tabsPic = Pictures
        [ tabBg (builderPaletteTab == TabBlocks) (leftX + panelW*0.25)
        , tabBg (builderPaletteTab == TabEnemies) (leftX + panelW*0.75)
        , translate (leftX + panelW*0.17) (topY - tabH*0.7) $ scale 0.15 0.15 $ color black $ text "Blocks"
        , translate (leftX + panelW*0.62) (topY - tabH*0.7) $ scale 0.15 0.15 $ color black $ text "Enemies"
        ]
      items = case builderPaletteTab of { TabBlocks -> paletteItemsBlocks; TabEnemies -> paletteItemsEnemies }
      n = length items
      cols = 2 :: Int
      rows = (n + cols - 1) `div` cols
      cellW = panelW / fromIntegral cols
      slotH = (sh - tabH) / fromIntegral (max 1 rows)
      iconSize = min (cellW * 0.8) (slotH * 0.8)
      scaleTo sz = let s = sz / assetTilePixelSize in scale s s
      getTileSprite m t = Map.findWithDefault blank t m
      -- grid position (row, col) to center coords
      cellCenter r c = let cx = leftX + (fromIntegral c + 0.5) * cellW
                           cy = (topY - tabH)  - (fromIntegral r + 0.5) * slotH
                       in (cx, cy)
      isSelected item = case item of
        PTile t      -> builderBrushMode == BrushNormal && t == builderBrush
        PSpecial sb  -> case sb of
                          GrassColumn -> builderBrushMode == BrushGrassColumn
                          Eraser      -> builderBrushMode == BrushEraser
        PEnemyGoomba -> builderPaletteTab == TabEnemies && builderEnemySel == EnemyGoomba
        PEnemyCoin   -> builderPaletteTab == TabEnemies && builderEnemySel == EnemyCoin
        PEnemyEraser -> builderPaletteTab == TabEnemies && builderEnemySel == EnemyEraser
      drawItem (i, item) =
        let c = i `div` rows
            r = i `mod` rows
            (cx, cy) = cellCenter r c
            selRect = if isSelected item then let half = iconSize/2 + 6
                                              in color black (lineLoop [(-half,-half),(half,-half),(half,half),(-half,half)])
                       else blank
            pic = case item of
              PTile t     -> scaleTo iconSize (getTileSprite tileMap t)
              PSpecial GrassColumn ->
                let halfSz = iconSize / 2
                    grassPic = scaleTo halfSz (getTileSprite tileMap Grass)
                    earthPic = scaleTo halfSz (getTileSprite tileMap Earth)
                in Pictures [ translate 0 (halfSz/2) grassPic
                            , translate 0 (-halfSz/2) earthPic ]
              PSpecial Eraser ->
                let half = iconSize / 2 * 0.8
                    border = color black (lineLoop [(-half,-half),(half,-half),(half,half),(-half,half)])
                    diag   = color (makeColor 1 0 0 0.9) (line [(-half,-half),(half,half)])
                in Pictures [border, diag]
              PEnemyGoomba ->
                let anim = getEntityAnim animMap TGoomba
                in scaleTo iconSize (head anim)
              PEnemyCoin ->
                let anim = getEntityAnim animMap TCoin
                in scaleTo iconSize (head anim)
              PEnemyEraser ->
                let half = iconSize / 2 * 0.8
                    border = color black (lineLoop [(-half,-half),(half,-half),(half,half),(-half,half)])
                    diag   = color (makeColor 1 0 0 0.9) (line [(-half,-half),(half,half)])
                in Pictures [border, diag]
        in translate cx cy (Pictures [selRect, pic])
  in Pictures (panelBg : tabsPic : map drawItem (zip [0..] items))

-- Keep the palette order in sync with Input
data SpecialBrush = GrassColumn | Eraser

data PaletteItem = PTile Tile | PSpecial SpecialBrush | PEnemyGoomba | PEnemyCoin | PEnemyEraser

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
  , PEnemyCoin ]

renderBuilderEntities :: Float -> AnimMap -> [Entity] -> Picture
renderBuilderEntities tilePixels animMap ents =
  Pictures (map (renderEntity tilePixels animMap 0) ents)

-- Leave confirmation popup overlay
renderLeaveConfirm :: (Int, Int) -> Picture
renderLeaveConfirm (screenW, screenH) =
  let sw = fromIntegral screenW :: Float
      sh = fromIntegral screenH :: Float
      -- darken background
      bg = color (makeColor 0 0 0 0.5) $ polygon [(-sw/2,-sh/2),(sw/2,-sh/2),(sw/2,sh/2),(-sw/2,sh/2)]
      panelW = sw * 0.6
      panelH = sh * 0.3
      panelRect = color (makeColor 1 1 1 0.9) $ polygon [(-panelW/2,-panelH/2),(panelW/2,-panelH/2),(panelW/2,panelH/2),(-panelW/2,panelH/2)]
      panelBorder = color black $ lineLoop [(-panelW/2,-panelH/2),(panelW/2,-panelH/2),(panelW/2,panelH/2),(-panelW/2,panelH/2)]
      -- text
      msg = "Are you sure you want to leave?\nUnsaved changes will be lost."
      textScale = (panelH * 0.12) / 100
      msgPic = color black $ scale textScale textScale $ translate (-panelW*0.28) (panelH*0.1) $ text msg
      -- buttons
      btnW = 220; btnH = 80
      btnY = - panelH * 0.15
      yesX = - panelW * 0.2
      noX  =   panelW * 0.2
      button label cx cy =
        let rect = polygon [(-btnW/2,-btnH/2),(btnW/2,-btnH/2),(btnW/2,btnH/2),(-btnW/2,btnH/2)]
            border = lineLoop [(-btnW/2,-btnH/2),(btnW/2,-btnH/2),(btnW/2,btnH/2),(-btnW/2,btnH/2)]
            s = (btnH * 0.45) / 100
            lbl = translate (- (fromIntegral (length label) * 7) * s) (-12) $ scale s s $ text label
        in translate cx cy $ Pictures [ color (makeColor 1 1 1 0.85) rect, color black border, color black lbl ]
      yesBtn = button "Yes" yesX btnY
      noBtn  = button "No"  noX  btnY
  in Pictures [bg, panelRect, panelBorder, msgPic, yesBtn, noBtn]

renderBuilderPreview :: Float -> BuilderState -> Picture
renderBuilderPreview tilePixels BuilderState { builderWorld = world
                                             , builderTileMap = tileMap
                                             , builderAnimMap = animMap
                                             , builderLastMouse = (mx, my)
                                             , builderCam = (camX, camY)
                                             , builderBrush
                                             , builderBrushMode
                                             , builderPaletteTab
                                             , builderEnemySel
                                             , builderScreenSize = (screenW, screenH)
                                             } =
  let -- Hide preview when hovering over the right-side palette panel
      sw = fromIntegral screenW :: Float
      sh = fromIntegral screenH :: Float
      panelW = sw / 6
      leftX  =  sw/2 - panelW
      rightX =  sw/2
      topY   =  sh/2
      bottomY = -sh/2
      hoveringPalette = mx >= leftX && mx <= rightX && my >= bottomY && my <= topY
  in if hoveringPalette || (builderPaletteTab == TabBlocks && builderBrushMode == BrushEraser) then blank else
    let wx = (mx - camX) / tilePixels
        wy = (my - camY) / tilePixels
        x  = floor wx
        y  = floor ((-wy))
        rows = grid world
        inBounds = y >= 0 && x >= 0 && y < length rows && x < length (head rows)
        getTileSprite m t = Map.findWithDefault blank t m
    in if not inBounds then blank else
      let xWorld = fromIntegral x + 0.5
          yWorld = negate (fromIntegral y) - 0.5
      in case builderPaletteTab of
           TabBlocks ->
             let t' = renderedTileFor rows x y builderBrush
                 sprite = withTileScale tilePixels (getTileSprite tileMap t')
             in translate (xWorld * tilePixels) (yWorld * tilePixels)
                  (color (makeColor 1 1 1 0.5) sprite)
           TabEnemies ->
             case builderEnemySel of
               EnemyGoomba ->
                 let anim = getEntityAnim animMap TGoomba
                     sprite = withTileScale tilePixels (head anim)
                 in translate (xWorld * tilePixels) (yWorld * tilePixels)
                      (color (makeColor 1 1 1 0.85) sprite)
               EnemyCoin ->
                 let anim = getEntityAnim animMap TCoin
                     sprite = withTileScale tilePixels (head anim)
                 in translate (xWorld * tilePixels) (yWorld * tilePixels)
                      (color (makeColor 1 1 1 0.85) sprite)
               EnemyEraser -> blank

renderBuilderWorld :: Float -> World -> TileMap -> Bool -> Picture
renderBuilderWorld tilePixels world tileMap debugMode =
  let 
    getTileSprite :: TileMap -> Tile -> Picture
    getTileSprite m t = Map.findWithDefault blank t m

    tilesPic = Pictures
      [ let t' = case tile of
                    _ -> renderedTileFor (grid world) x y tile
            xWorld = fromIntegral x + 0.5
            yWorld = negate (fromIntegral y) - 0.5
        in translate (xWorld * tilePixels) (yWorld * tilePixels)
             (withTileScale tilePixels (getTileSprite tileMap t'))
      | (y, row)  <- zip ([0..] :: [Int]) $ grid world
      , (x, tile) <- zip ([0..] :: [Int]) row
      ]
    colliderPics
      | debugMode = [Pictures (map (renderAABB tilePixels) (colliders world))]
      | otherwise = []
    -- Red overlay for non-drawable regions (outside current grid bounds)
    widthPx  = fromIntegral (length (head (grid world))) * tilePixels
    heightPx = fromIntegral (length (grid world)) * tilePixels
    big      = 1000 * tilePixels
    faintRed = makeColor 1 0 0 0.25
    poly ps  = color faintRed (polygon ps)
    -- Areas: left of x=0, above y=0, right of x=widthPx, below y=-heightPx
    leftArea   = poly [(-big,  big), (0,    big), (0,   -big), (-big, -big)]
    topArea    = poly [(0,     big), (widthPx, big), (widthPx, 0), (0, 0)]
    rightArea  = poly [(widthPx, big), (big,  big), (big, -big), (widthPx, -big)]
    bottomArea = poly [(0, -heightPx), (widthPx, -heightPx), (widthPx, -big), (0, -big)]
    redOverlay = Pictures [leftArea, topArea, rightArea, bottomArea]
  in Pictures ([redOverlay, tilesPic] ++ colliderPics)