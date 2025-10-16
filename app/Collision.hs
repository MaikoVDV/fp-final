module Collision
  ( Collider(..)
--   , getAABB
  , collides
--   , getLevelCollisionsFor
  , generateCollidersForWorld
  , entityCollidersForState
  ) where

import Model
import Data.Maybe (catMaybes)

-- Build an AABB from a Movable entity (adjust sizes per entity as needed)
-- getAABB :: Movable e => e -> Collider
-- getAABB e = AABB (getPos e) 0.9 0.9 -- replace widths/heights with appropriate values

-- simple AABB vs AABB overlap test (treat widths/heights as full sizes)
collides :: Collider -> Collider -> Bool
collides (AABB (x1,y1) w1 h1) (AABB (x2,y2) w2 h2) =
  abs (x1 - x2) * 2 < (w1 + w2) && abs (y1 - y2) * 2 < (h1 + h2)

-- compute which tiles an entity collides with (TileMap type from Model)
-- getLevelCollisionsFor :: (Movable e) => e -> TileMap -> [TileMap]
-- getLevelCollisionsFor e tiles = 
--   -- implement tile lookup / filtering using getAABB and collides
--   [] -- stub: return list of colliding tiles

generateCollidersForWorld :: [[Tile]] -> [Collider]
generateCollidersForWorld rows =
  [ AABB (fromIntegral x + 0.5, negate (fromIntegral y) - 0.5) 1 1
  | (y, row) <- zip ([0..] :: [Int]) rows
  , (x, tile) <- zip ([0..] :: [Int]) row
  , isSolid tile
  ]
  where
    isSolid Air = False
    isSolid _   = True

entityCollidersForState :: GameState -> [Collider]
entityCollidersForState GameState { player, entities } =
  catMaybes (playerCollider player : map entityCollider entities)
