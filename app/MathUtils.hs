module MathUtils where

import Graphics.Gloss
import Model.Config

tangentFromNormal :: Vector -> Vector
tangentFromNormal (nx, ny) = normalizeVec (-ny, nx)

dotVec :: Vector -> Vector -> Float
dotVec (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

normalizeVec :: Vector -> Vector
normalizeVec (x, y) =
  let mag = sqrt (x * x + y * y)
  in if mag < 1e-6 then (0, 0) else (x / mag, y / mag)

addVec :: Vector -> Vector -> Vector
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Applies scalar to a vector. Useful for (among other things) scaling physics vectors to deltatime
scaleVec :: Vector -> Float -> Vector
scaleVec (x, y) s = (x * s, y * s)

-- Moves a point along a vector
addVecToPoint :: Point -> Vector -> Point
addVecToPoint (x, y) (dx, dy) = (x + dx, y + dy)

subPoints :: Point -> Point -> Vector
subPoints (x1, y1) (x0, y0) = (x1 - x0, y1 - y0)

negateVec :: Vector -> Vector
negateVec (x, y) = (-x, -y)

upVector :: Vector
upVector = (0, 1)

clampTileZoom :: Float -> Float
clampTileZoom n = max minTileZoom (min maxTileZoom n)

clamp01 :: Float -> Float
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x

pointInBounds :: (Float, Float, Float, Float) -> Point -> Bool
pointInBounds (minX, maxX, minY, maxY) (x, y) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

withinBounds :: Float -> Float -> Bool
withinBounds x maxX = x >= 0 && x < maxX