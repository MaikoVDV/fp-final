module View where

import Graphics.Gloss
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)

import Model.Types
import Model.Collider
import Model.InitialState

assetTilePixelSize :: Float
assetTilePixelSize = 18

scaleFactor :: Float
scaleFactor = 0.5

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

viewGame :: GameState -> Picture
viewGame gs =
  let tilePixels = tilePixelsForState gs
      (px, py) = playerPos (player gs)
      (_, screenHeightInt) = screenSize gs
      screenHeight = fromIntegral screenHeightInt
      desiredPlayerScreenFraction = 1 / 3 :: Float
      targetPlayerY = (-0.5 + desiredPlayerScreenFraction) * screenHeight
      camX = -px * tilePixels
      camY = targetPlayerY - (py * tilePixels)
  in translate camX camY $
       Pictures
         [ renderWorld tilePixels gs
         , renderEntities tilePixels gs
         , renderPlayer tilePixels (player gs)
         ]

renderMenu :: MenuState -> Picture
renderMenu MenuState { menuDebugMode } =
  let titleText =
        color white $
          translate (-260) 80 $
            scale 0.35 0.35 $
              text "FP Final"
      promptText =
        color white $
          translate (-280) (-40) $
            scale 0.2 0.2 $
              text "Press Enter to start"
      debugText
        | menuDebugMode =
            color yellow $
              translate (-285) (-120) $
                scale 0.16 0.16 $
                  text "Debug mode enabled"
        | otherwise = blank
  in Pictures [titleText, promptText, debugText]

renderWorld :: Float -> GameState -> Picture
renderWorld tilePixels GameState { world, tileMap, player, entities, debugMode } =
  let tilesPic = Pictures
        [ translate (xWorld * tilePixels) (yWorld * tilePixels) (withTileScale tilePixels (getTileSprite tileMap tile))
        | (y, row)  <- zip ([0..] :: [Int]) $ grid world
        , (x, tile) <- zip ([0..] :: [Int]) row
        , let xWorld = fromIntegral x + 0.5
        , let yWorld = negate (fromIntegral y) - 0.5
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
renderEntities tilePixels GameState { entities } = Pictures $ map (renderEntity tilePixels) entities

renderEntity :: Float -> Entity -> Picture
renderEntity tilePixels (EGoomba _ g)       = renderGoomba tilePixels g
renderEntity _          (EKoopa _ k)        = renderKoopa  k
renderEntity _          (EPowerup _)        = blank
renderEntity _          (EMovingPlatform _) = blank

getTileSprite :: TileMap -> Tile -> Picture
getTileSprite m t = Map.findWithDefault blank t m

-- Deze kunnen we misschien allemaal in een RenderEntity functie stoppen
-- door bijv. alle entities een Renderable typeclass te geven waarin
-- een functie staat zoals getSprite of getAnimation of zoiets
renderPlayer :: Float -> Player -> Picture
renderPlayer tilePixels Player { playerPos = (x, y), playerSprite } = 
  let sprite = head playerSprite
      spriteScaled = withTileScale tilePixels sprite
  in translate (x * tilePixels) (y * tilePixels) spriteScaled -- dit klopt nog niet helemaal met animaties enz

renderGoomba :: Float -> Goomba -> Picture
renderGoomba tilePixels Goomba { goombaPos = (x, y) } =
  let w = 0.9 * tilePixels
      h = 0.9 * tilePixels
  in translate (x * tilePixels) (y * tilePixels) $
       color red $
         rectangleSolid w h

renderKoopa :: Koopa -> Picture
renderKoopa = undefined

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
