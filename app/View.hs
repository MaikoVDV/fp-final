module View where

import Graphics.Gloss
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)

import Model

baseTilePixelSize :: Float
baseTilePixelSize = 18

tileScaleFactor :: Int -> Float
tileScaleFactor size = fromIntegral size / baseTilePixelSize

withTileScale :: Int -> Picture -> Picture
withTileScale size pic =
  let s = tileScaleFactor size
  in scale s s pic

view :: AppState -> IO Picture
view = return . viewPure

viewPure :: AppState -> Picture
viewPure (Menu menuState) = renderMenu menuState
viewPure (Playing gs) = viewGame gs

viewGame :: GameState -> Picture
viewGame gs =
  let tileSizeF = fromIntegral (tileSize gs)
      (px, py) = playerPos (player gs)
      camX = -px * tileSizeF
      camY = -py * tileSizeF
  in translate camX camY $
       Pictures
         [ renderWorld gs
         , renderEntities (tileSize gs) gs
         , renderPlayer (tileSize gs) (player gs)
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

renderWorld :: GameState -> Picture
renderWorld GameState { world, tileMap, tileSize, player, entities, debugMode } =
  let tileSizeF = fromIntegral tileSize
      tilesPic = Pictures
        [ translate (xWorld * tileSizeF) (yWorld * tileSizeF) (withTileScale tileSize (getTileSprite tileMap tile))
        | (y, row)  <- zip ([0..] :: [Int]) $ grid world
        , (x, tile) <- zip ([0..] :: [Int]) row
        , let xWorld = fromIntegral x + 0.5
        , let yWorld = negate (fromIntegral y) - 0.5
        ]
      colliderPics
        | debugMode =
            let worldCollidersPics = map (renderAABB tileSizeF) (colliders world)
                playerColliderPic  = map (renderAABB tileSizeF) (maybeToList (playerCollider player))
                entityColliderPics = map (renderAABB tileSizeF) (mapMaybe entityCollider entities)
            in [Pictures (worldCollidersPics ++ playerColliderPic ++ entityColliderPics)]
        | otherwise = []
  in Pictures (tilesPic : colliderPics)

renderEntities :: Int -> GameState -> Picture
renderEntities tileSize GameState { entities } = Pictures $ map (renderEntity tileSize) entities

renderEntity :: Int -> Entity -> Picture
renderEntity tileSize (EPlayer p)     = renderPlayer tileSize p
renderEntity tileSize (EGoomba g)     = renderGoomba tileSize g
renderEntity _        (EKoopa  k)     = renderKoopa  k
renderEntity _        EPowerup        = blank
renderEntity _        EMovingPlatform = blank

getTileSprite :: TileMap -> Tile -> Picture
getTileSprite m t = Map.findWithDefault blank t m

-- Deze kunnen we misschien allemaal in een RenderEntity functie stoppen
-- door bijv. alle entities een Renderable typeclass te geven waarin
-- een functie staat zoals getSprite of getAnimation of zoiets
renderPlayer :: Int -> Player -> Picture
renderPlayer tileSize Player { playerPos = (x, y), playerSprite } = 
  let sprite = head playerSprite
      tileSizePixels = fromIntegral tileSize
      spriteScaled = withTileScale tileSize sprite
  in translate (x * tileSizePixels) (y * tileSizePixels) spriteScaled -- dit klopt nog niet helemaal met animaties enz

renderGoomba :: Int -> Goomba -> Picture
renderGoomba tileSize Goomba { goombaPos = (x, y) } =
  let ts = fromIntegral tileSize
      w = 0.9 * ts
      h = 0.9 * ts
  in translate (x * ts) (y * ts) $
       color red $
         rectangleSolid w h

renderKoopa :: Koopa -> Picture
renderKoopa = undefined

renderAABB :: Float -> Collider -> Picture
renderAABB tileSize (AABB (x, y) w h) =
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
