module View.Game where

import Graphics.Gloss
import qualified Data.Map as Map
import Data.Maybe

import Model.Types
import Model.TypesState
import Model.Collider
import View.GameUI
import View.Entity
import View.Helpers

viewGame :: GameState -> Picture
viewGame gs@GameState { player, screenSize } =
  let tilePixels = tilePixelsForState gs
      (px, py) = playerPos player
      worldRows = grid (world gs)
      worldHeight = length worldRows
      (screenWidthInt, screenHeightInt) = screenSize
      screenWidth  = fromIntegral screenWidthInt
      screenHeight = fromIntegral screenHeightInt
      desiredPlayerScreenFraction = 1 / 3 :: Float
      targetPlayerY = (-0.5 + desiredPlayerScreenFraction) * screenHeight
      halfWidthTiles = screenWidth / (2 * tilePixels)
      clampedPx = max halfWidthTiles px
      camX = -(clampedPx * tilePixels)
      rawCamY = targetPlayerY - (py * tilePixels)
      maxCamY =
        if worldHeight <= 0
          then rawCamY
          else fromIntegral worldHeight * tilePixels - screenHeight / 2
      camY = min rawCamY maxCamY
      visibleBounds = expandBounds (visibleBoundsFromCamera tilePixels screenWidth screenHeight camX camY) 2.0
      
      worldPic = translate camX camY $
        Pictures
          [ renderWorld tilePixels visibleBounds gs 
          , renderEntities tilePixels visibleBounds gs
          , renderPlayer tilePixels player
          ]
      hudPic = renderHUD screenWidth screenHeight gs
      pauseOverlay = if paused gs then renderPauseMenu (screenWidthInt, screenHeightInt) else blank
  in Pictures [ worldPic, hudPic, pauseOverlay ]

renderWorld :: Float -> VisibleBounds -> GameState -> Picture
renderWorld tilePixels bounds GameState { world, tileMap, player, entities, debugMode } =
  case grid world of
    [] -> blank
    rows ->
      let height = length rows
          width  = length (head rows)
          (rowStart, rowEnd) = visibleRowRange bounds height
          (colStart, colEnd) = visibleColRange bounds width
          rowIndices = [rowStart.. rowEnd]
          getTileSprite :: TileMap -> Tile -> Picture
          getTileSprite m t = Map.findWithDefault blank t m
          tilesPic = Pictures
            [ let t' = renderedTileFor rows x y tile
                  xWorld = fromIntegral x + 0.5
                  yWorld = negate (fromIntegral y) - 0.5
              in translate (xWorld * tilePixels) (yWorld * tilePixels)
                   (withTileScale tilePixels (getTileSprite tileMap t'))
            | y <- rowIndices
            , let row = rows !! y
            , let rowLen = length row
            , rowLen > 0
            , let startX = max colStart 0
            , let endX = min colEnd (rowLen - 1)
            , startX <= endX
            , let cols = [startX .. endX]
            , x <- cols
            , let tile = row !! x
            ]
          colliderPics
            | debugMode =
                let colliderBounds = expandBounds bounds 2
                    worldCollidersPics = map (renderAABB tilePixels) (filter (colliderVisible colliderBounds) (colliders world))
                    playerColliderPic  = map (renderAABB tilePixels) (maybeToList (playerCollider player))
                    entityColliderPics = map (renderAABB tilePixels) (mapMaybe entityCollider (filter (entityVisible bounds) entities))
                in [Pictures (worldCollidersPics ++ playerColliderPic ++ entityColliderPics)]
            | otherwise = []
      in Pictures (tilesPic : colliderPics)

renderEntities :: Float -> VisibleBounds -> GameState -> Picture
renderEntities tilePixels bounds GameState { entities, animMap, frameCount } =
  let visible = filter (entityVisible bounds) entities
  in Pictures (map (renderEntity tilePixels animMap frameCount) visible)
