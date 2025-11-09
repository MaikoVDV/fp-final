module View.Helpers where

import Graphics.Gloss

import Model.Types
import Model.TypesState
import Model.Config
import Model.InitialState
import qualified Model.Types as Types

tilePixelsForState :: GameState -> Float
tilePixelsForState GameState { tileZoom, screenSize } =
  baseTilePixelSizeForScreen screenSize * tileZoom * scaleFactor

tileScaleFactor :: Float -> Float
tileScaleFactor tilePixels = tilePixels / assetTilePixelSize

withTileScale :: Float -> Picture -> Picture
withTileScale tilePixels pic =
  let s = tileScaleFactor tilePixels
  in scale s s pic

expandBounds :: VisibleBounds -> Float -> VisibleBounds
expandBounds VisibleBounds { vbMinX, vbMaxX, vbMinY, vbMaxY } extra =
  VisibleBounds
    { vbMinX = vbMinX - extra
    , vbMaxX = vbMaxX + extra
    , vbMinY = vbMinY - extra
    , vbMaxY = vbMaxY + extra
    }

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


withScale :: (Float, Float) -> Picture -> Picture
withScale (sx,sy) = scale sx sy

visibleBoundsFromCamera :: Float -> Float -> Float -> Float -> Float -> VisibleBounds
visibleBoundsFromCamera tilePixels screenWidth screenHeight camX camY =
  VisibleBounds
    { vbMinX = (-screenWidth / 2 - camX) / tilePixels
    , vbMaxX = ( screenWidth / 2 - camX) / tilePixels
    , vbMinY = (-screenHeight / 2 - camY) / tilePixels
    , vbMaxY = ( screenHeight / 2 - camY) / tilePixels
    }

visibleRowRange :: VisibleBounds -> Int -> (Int, Int)
visibleRowRange _ height | height <= 0 = (0, -1)
visibleRowRange VisibleBounds { vbMinY, vbMaxY } height =
  let rawStart = ceiling (-vbMaxY - 0.5)
      rawEnd   = floor   (-vbMinY - 0.5)
      start = max 0 rawStart
      end   = min (height - 1) rawEnd
  in if start > end then (0, -1) else (start, end)

visibleColRange :: VisibleBounds -> Int -> (Int, Int)
visibleColRange _ width | width <= 0 = (0, -1)
visibleColRange VisibleBounds { vbMinX, vbMaxX } width =
  let rawStart = floor (vbMinX - 1)
      rawEnd   = ceiling (vbMaxX + 1)
      start = max 0 rawStart
      end   = min (width - 1) rawEnd
  in if start > end then (0, -1) else (start, end)

rangeList :: (Int, Int) -> [Int]
rangeList (a, b)
  | a > b     = []
  | otherwise = [a .. b]

pointWithin :: VisibleBounds -> Point -> Bool
pointWithin VisibleBounds { vbMinX, vbMaxX, vbMinY, vbMaxY } (x, y) =
  x >= vbMinX && x <= vbMaxX && y >= vbMinY && y <= vbMaxY

entityWorldPos :: Entity -> Maybe Point
entityWorldPos (EGoomba _ Goomba { goombaPos }) = Just goombaPos
entityWorldPos (EKoopa  _ Koopa  { koopaPos  }) = Just koopaPos
entityWorldPos (EPowerup _ Powerup { powerupPos }) = Just powerupPos
entityWorldPos (ECoin    _ Coin    { coinPos    }) = Just coinPos
entityWorldPos _ = Nothing

entityVisible :: VisibleBounds -> Entity -> Bool
entityVisible bounds entity =
  case entityWorldPos entity of
    Just pos -> pointWithin (expandBounds bounds 1.5) pos
    Nothing  -> True

colliderVisible :: VisibleBounds -> Collider -> Bool
colliderVisible bounds AABB { aPos = (cx, cy), aWidth, aHeight } =
  let halfW = aWidth / 2
      halfH = aHeight / 2
      left   = cx - halfW
      right  = cx + halfW
      bottom = cy - halfH
      top    = cy + halfH
      VisibleBounds { vbMinX, vbMaxX, vbMinY, vbMaxY } = expandBounds bounds 1.0
  in not (right < vbMinX || left > vbMaxX || top < vbMinY || bottom > vbMaxY)

-- Shared helper: decide which tile to render based on neighbors
-- Earth tiles render as a random Earth variant; Grass with ground above also renders as Earth variant
renderedTileFor :: [[Tile]] -> Int -> Int -> Tile -> Tile
renderedTileFor rows x y tile =
  case tile of
    Grass | hasGroundAbove x y rows -> earthVariantFor x y
    Earth                           -> earthVariantFor x y
    Earth2                          -> earthVariantFor x y
    _                               -> tile
  where
    isGround :: Tile -> Bool
    isGround Grass = True
    isGround Earth = True
    isGround Earth2 = True
    isGround _     = False

    hasGroundAbove :: Int -> Int -> [[Tile]] -> Bool
    hasGroundAbove _ yi _ | yi <= 0 = False
    hasGroundAbove xi yi g = isGround ((g !! (yi - 1)) !! xi)

    -- Deterministic 50/50 variant choice based on tile coords
    earthVariantFor :: Int -> Int -> Tile
    earthVariantFor xi yi = if ((xi * 31 + yi * 57) `mod` 2 == 0) then Earth else Earth2

dirToPictureScaleX :: MoveDir -> Float
dirToPictureScaleX DirLeft  = 1
dirToPictureScaleX DirRight = -1