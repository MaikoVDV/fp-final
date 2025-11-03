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
            headerBtn = buttonPic btnW btnH "New Level" (menuFocus == 0) (yOf 0)
            filesPic = case menuCustomFiles of
              [] -> [translate (-250) (yOf 1) $ color white $ scale 0.18 0.18 $ text "No levels found in ./levels"]
              fs -> [ buttonPic btnW btnH f (menuFocus == ix+1) (yOf (ix+1)) | (ix, f) <- zip [0..] fs ]
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
viewBuilder bs@BuilderState { builderWorld, builderTileMap, builderTileZoom, builderScreenSize, builderDebugMode, builderCam = (camX, camY) } =
  let tilePixels = baseTilePixelSizeForScreen builderScreenSize * builderTileZoom * scaleFactor
      worldPic   = renderBuilderWorld tilePixels builderWorld builderTileMap builderDebugMode
      previewPic = renderBuilderPreview tilePixels bs
  in translate camX camY (Pictures [worldPic, previewPic])

renderBuilderPreview :: Float -> BuilderState -> Picture
renderBuilderPreview tilePixels BuilderState { builderWorld = world, builderTileMap = tileMap, builderLastMouse = (mx, my), builderCam = (camX, camY), builderBrush } =
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
