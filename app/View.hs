module View where

import Graphics.Gloss
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord

import Model.Types
import qualified Model.Types as Types
import Model.Collider
import Model.InitialState
import Model.Config

-- Shared helper: decide which tile to render based on neighbors
-- If placing Grass and there is ground above, render Earth instead
renderedTileFor :: [[Tile]] -> Int -> Int -> Tile -> Tile
renderedTileFor rows x y tile =
  case tile of
    Grass | hasGroundAbove x y rows -> Earth
    _                               -> tile
  where
    isGround :: Tile -> Bool
    isGround Grass = True
    isGround Earth = True
    isGround _     = False

    hasGroundAbove :: Int -> Int -> [[Tile]] -> Bool
    hasGroundAbove _ yi _ | yi <= 0 = False
    hasGroundAbove xi yi g = isGround ((g !! (yi - 1)) !! xi)

tilePixelsForState :: GameState -> Float
tilePixelsForState GameState { tileZoom, screenSize } =
  baseTilePixelSizeForScreen screenSize * tileZoom * scaleFactor

tileScaleFactor :: Float -> Float
tileScaleFactor tilePixels = tilePixels / assetTilePixelSize

withTileScale :: Float -> Picture -> Picture
withTileScale tilePixels pic =
  let s = tileScaleFactor tilePixels
  in scale s s pic

view :: AppState -> IO Picture
view = return . viewPure

viewPure :: AppState -> Picture
viewPure (Menu menuState) = renderMenu menuState
viewPure (Playing gs) = viewGame gs
viewPure (Building bs) = viewBuilder bs

viewGame :: GameState -> Picture
viewGame gs@GameState { player, screenSize, frameCount } =
  let tilePixels = tilePixelsForState gs
      (px, py) = playerPos player
      (screenWidthInt, screenHeightInt) = screenSize
      screenWidth  = fromIntegral screenWidthInt
      screenHeight = fromIntegral screenHeightInt
      desiredPlayerScreenFraction = 1 / 3 :: Float
      targetPlayerY = (-0.5 + desiredPlayerScreenFraction) * screenHeight
      camX = -px * tilePixels
      camY = targetPlayerY - (py * tilePixels)
      worldPic = translate camX camY $
        Pictures
          [ renderWorld tilePixels gs 
          , renderEntities tilePixels gs
          , renderPlayer tilePixels player frameCount
          ]
  in Pictures [ worldPic, renderHUD screenWidth screenHeight gs ]

renderMenu :: MenuState -> Picture
renderMenu MenuState { menuDebugMode, menuScreenSize, menuFocus, menuPage, menuCustomFiles, menuInput } =
  let (screenW, screenH) = menuScreenSize
      sw = fromIntegral screenW
      sh = fromIntegral screenH
      titleScale = (sh * 0.12) / 100.0

      buttonPic btnW btnH label focused y =
        let bg    = if focused then makeColor 1 1 0 0.35 else makeColor 1 1 1 0.15
            borderCol = if focused then yellow else white
            rect = polygon [(-btnW/2, -btnH/2), (btnW/2, -btnH/2), (btnW/2, btnH/2), (-btnW/2, btnH/2)]
            border = lineLoop [(-btnW/2, -btnH/2), (btnW/2, -btnH/2), (btnW/2, btnH/2), (-btnW/2, btnH/2)]
            textScale = (btnH * 0.45) / 100.0
            labelPic = scale textScale textScale $ text label
            labelShift = - (fromIntegral (length label) * 7) * textScale
        in translate 0 y $ Pictures [ color bg rect, color borderCol border, translate labelShift (-12) (color white labelPic) ]

      debugPic
        | menuDebugMode = translate (-sw/2 + 10) (-sh/2 + 20) $ color yellow $ scale 0.12 0.12 $ text "Debug"
        | otherwise = blank

  in case menuPage of
      MainMenu ->
        let titlePic = translate (-sw * 0.25) (sh * 0.23) $ color white $ scale titleScale titleScale $ text "FP Final"
            btnW = 460 :: Float
            btnH = 90  :: Float
            ys   = [100, 0, -100] :: [Float]
            labels = ["Play", "Builder", "Custom Levels"]
            buttonsPic = Pictures
              [ buttonPic btnW btnH label (menuFocus == ix) y
              | (ix, (label, y)) <- zip [0..] (zip labels ys)
              ]
        in Pictures [titlePic, buttonsPic, debugPic]

      CustomLevels ->
        let titlePic = translate (-sw * 0.33) (sh * 0.23) $ color white $ scale titleScale titleScale $ text "Custom Levels"
            btnW = 600 :: Float
            btnH = 70  :: Float
            yOf i = 120 - fromIntegral i * 80 :: Float
            items = case menuCustomFiles of
              [] -> [translate (-200) 20 $ color white $ scale 0.18 0.18 $ text "No levels found in ./levels"]
              fs -> [ buttonPic btnW btnH f (menuFocus == ix) (yOf ix) | (ix, f) <- zip [0..] fs ]
        in Pictures ([titlePic] ++ items ++ [debugPic])

      BuilderSelect ->
        let titlePic = translate (-sw * 0.33) (sh * 0.23) $ color white $ scale titleScale titleScale $ text "Builder: Select Level"
            btnW = 600 :: Float
            btnH = 70  :: Float
            yOf i = 160 - fromIntegral i * 80 :: Float
            -- Decode focus to (row, col)
            focusRow = menuFocus `div` 2
            focusCol = menuFocus `mod` 2
            headerBtn = buttonPic btnW btnH "New Level" (focusRow == 0) (yOf 0)
            filesPic = case menuCustomFiles of
              [] -> [translate (-250) (yOf 1) $ color white $ scale 0.18 0.18 $ text "No levels found in ./levels"]
              fs ->
                let delW = 160 :: Float
                    delH = btnH
                    delX = btnW/2 + 40 + delW/2  -- right of the file button
                    delLabelScale = (delH * 0.45) / 100.0
                    delRect = polygon [(-delW/2, -delH/2), (delW/2, -delH/2), (delW/2, delH/2), (-delW/2, delH/2)]
                    delBorder = lineLoop [(-delW/2, -delH/2), (delW/2, -delH/2), (delW/2, delH/2), (-delW/2, delH/2)]
                    delPic focused y =
                      let bg = if focused then makeColor 1 0.6 0.6 0.5 else makeColor 1 0 0 0.25
                          br = if focused then makeColor 1 0.3 0.3 1 else makeColor 1 0 0 1
                      in translate delX y $ Pictures [ color bg delRect
                                                      , color br delBorder
                                                      , translate (-30) (-12) $ color white $ scale delLabelScale delLabelScale $ text "Delete"
                                                      ]
                in [ Pictures [ buttonPic btnW btnH f (focusRow == ix+1 && focusCol == 0) (yOf (ix+1))
                              , delPic (focusRow == ix+1 && focusCol == 1) (yOf (ix+1))
                              ]
                   | (ix, f) <- zip [0..] fs
                   ]
        in Pictures ([titlePic, headerBtn] ++ filesPic ++ [debugPic])

      BuilderName ->
        let titlePic = translate (-sw * 0.33) (sh * 0.23) $ color white $ scale titleScale titleScale $ text "Builder: New Level"
            prompt  = translate (-sw * 0.35) 40 $ color white $ scale 0.2 0.2 $ text "Enter level name:"
            inputBxW = 800; inputBxH = 80
            inputRect = color (makeColor 1 1 1 0.15) $ translate 0 (-20) $ polygon [(-inputBxW/2,-inputBxH/2),(inputBxW/2,-inputBxH/2),(inputBxW/2,inputBxH/2),(-inputBxW/2,inputBxH/2)]
            label    = let s = menuInput
                           scaleT = 0.2
                           shiftX = - (fromIntegral (length s) * 7) * scaleT
                       in translate shiftX (-35) $ color white $ scale scaleT scaleT $ text s
            hint     = translate (-sw * 0.28) (-120) $ color white $ scale 0.15 0.15 $ text "Enter to confirm | Esc to cancel"
        in Pictures [titlePic, prompt, inputRect, label, hint, debugPic]

-- Builder view
viewBuilder :: BuilderState -> Picture
viewBuilder bs@BuilderState { builderWorld, builderTileMap, builderTileZoom, builderScreenSize, builderDebugMode, builderCam = (camX, camY), builderConfirmLeave } =
  let tilePixels = baseTilePixelSizeForScreen builderScreenSize * builderTileZoom * scaleFactor
      worldPic   = renderBuilderWorld tilePixels builderWorld builderTileMap builderDebugMode
      previewPic = renderBuilderPreview tilePixels bs
      palettePic = renderBuilderPalette bs
      confirmPic = if builderConfirmLeave then renderLeaveConfirm builderScreenSize else blank
  in Pictures [ translate camX camY (Pictures [worldPic, previewPic])
              , palettePic
              , confirmPic
              ]

-- Right-side palette UI to select builder brush
renderBuilderPalette :: BuilderState -> Picture
renderBuilderPalette BuilderState { builderScreenSize = (screenW, screenH), builderTileMap = tileMap, builderBrush, builderBrushMode } =
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
      items = paletteItems
      n = length items
      cols = 2 :: Int
      rows = (n + cols - 1) `div` cols
      cellW = panelW / fromIntegral cols
      slotH = sh / fromIntegral (max 1 rows)
      iconSize = min (cellW * 0.8) (slotH * 0.8)
      scaleTo sz = let s = sz / assetTilePixelSize in scale s s
      getTileSprite m t = Map.findWithDefault blank t m
      -- grid position (row, col) to center coords
      cellCenter r c = let cx = leftX + (fromIntegral c + 0.5) * cellW
                           cy = topY  - (fromIntegral r + 0.5) * slotH
                       in (cx, cy)
      isSelected item = case item of
        PTile t      -> builderBrushMode == BrushNormal && t == builderBrush
        PSpecial sb  -> case sb of
                          GrassColumn -> builderBrushMode == BrushGrassColumn
                          Eraser      -> builderBrushMode == BrushEraser
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
        in translate cx cy (Pictures [selRect, pic])
  in Pictures (panelBg : map drawItem (zip [0..] items))

-- Keep the palette order in sync with Input
data SpecialBrush = GrassColumn | Eraser

data PaletteItem = PTile Tile | PSpecial SpecialBrush

paletteItems :: [PaletteItem]
paletteItems =
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
                                             , builderLastMouse = (mx, my)
                                             , builderCam = (camX, camY)
                                             , builderBrush
                                             , builderBrushMode
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
  in if hoveringPalette || builderBrushMode == BrushEraser then blank else
    let wx = (mx - camX) / tilePixels
        wy = (my - camY) / tilePixels
        x  = floor wx
        y  = floor ((-wy))
        rows = grid world
        inBounds = y >= 0 && x >= 0 && y < length rows && x < length (head rows)
        getTileSprite m t = Map.findWithDefault blank t m
    in if not inBounds
          then blank
          else
            let t' = renderedTileFor rows x y builderBrush
                xWorld = fromIntegral x + 0.5
                yWorld = negate (fromIntegral y) - 0.5
                sprite = withTileScale tilePixels (getTileSprite tileMap t')
            in translate (xWorld * tilePixels) (yWorld * tilePixels)
                 (color (makeColor 1 1 1 0.5) sprite)

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

renderWorld :: Float -> GameState -> Picture
renderWorld tilePixels GameState { world, tileMap, player, entities, debugMode } =
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
      | debugMode =
          let worldCollidersPics = map (renderAABB tilePixels) (colliders world)
              playerColliderPic  = map (renderAABB tilePixels) (maybeToList (playerCollider player))
              entityColliderPics = map (renderAABB tilePixels) (mapMaybe entityCollider entities)
          in [Pictures (worldCollidersPics ++ playerColliderPic ++ entityColliderPics)]
      | otherwise = []
  in Pictures (tilesPic : colliderPics)

renderEntities :: Float -> GameState -> Picture
renderEntities tilePixels GameState { entities, animMap, frameCount } = Pictures $ map (renderEntity tilePixels animMap frameCount) entities

renderEntity :: Float -> AnimMap -> Int -> Entity -> Picture
renderEntity tPx m fCtx (EGoomba   _ Goomba { goombaPos, goombaDir })   = renderEntity' tPx (getEntityAnim m TGoomba)  fCtx goombaPos  goombaDir
renderEntity tPx m fCtx (EKoopa    _ Koopa { koopaPos, koopaDir })      = renderEntity' tPx (getEntityAnim m TKoopa)   fCtx koopaPos   koopaDir
renderEntity tPx m fCtx (EPowerup  _ Powerup{ powerupPos, powerupDir }) = renderEntity' tPx (getEntityAnim m TPowerup) fCtx powerupPos powerupDir
renderEntity _   _ _    (EPlatform _)                                   = blank

renderEntity' :: Float -> Animation -> Int -> Point -> MoveDir -> Picture
renderEntity' tPx anim fCtx (x, y) dir = 
  let  
    spriteIdx = (fCtx `div` frameTime) `mod` length anim
    sprite = anim !! spriteIdx -- dit klopt nog niet helemaal met animaties enz
    spriteScaled = withTileScale tPx sprite
    spriteFlipped = scale (dirToPictureScaleX dir) 1 spriteScaled
  in translate (x * tPx) (y * tPx) spriteFlipped 

getEntityAnim :: AnimMap -> EntityType -> Animation
getEntityAnim m t = Map.findWithDefault [blank] t m

renderPlayer :: Float -> Player -> Int -> Picture
renderPlayer tilePixels p@Player { playerPos = (x, y), health, playerAnim = anims, lastMoveDir } fCtx = 
  let 
    moveDir = if lastMoveDir == -1 then 1 else -1
    anim = anims !! clamp (0, length anims - 1) (health - 1)
    movingInput = moveLeftHeld p || moveRightHeld p
    clock = playerAnimClock p
    spriteIdx = if movingInput then (floor clock `mod` length anim) else 0
    sprite = anim !! spriteIdx
    spriteScaled = withTileScale tilePixels sprite
    spriteFlipped = scale moveDir 1 spriteScaled
  in translate (x * tilePixels) (y * tilePixels) spriteFlipped

renderAABB :: Float -> Collider -> Picture
renderAABB tileSize (AABB (x, y) w h _) =
  let xPixels = x * tileSize
      yPixels = y * tileSize
      halfW   = (w * tileSize) / 2
      halfH   = (h * tileSize) / 2
      corners =
        [ (-halfW,  halfH)
        , ( halfW,  halfH)
        , ( halfW, -halfH)
        , (-halfW, -halfH)
        ]
  in translate xPixels yPixels $
       color green $
         lineLoop corners

dirToPictureScaleX :: MoveDir -> Float
dirToPictureScaleX Types.Left  = 1
dirToPictureScaleX Types.Right = -1

-- Simple HUD: show jumps remaining in top-left corner
renderHUD :: Float -> Float -> GameState -> Picture
renderHUD screenW screenH GameState { player, debugMode } =
  if not debugMode then blank else
    let margin = 20
        x = -screenW / 2 + margin
        -- Place text fully within the screen: subtract its scaled height
        s = (screenH * 0.08) / 100.0  -- ~8% of screen height
        textHeight = 100 * s          -- Gloss baseline ~100 units
        y =  screenH / 2 - margin - textHeight
        txt = "Jumps: " ++ show (jumpsLeft player)
    in translate x y $ color white $ scale s s $ text txt
