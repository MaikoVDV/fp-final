module Model.World where

import Model.Types
import Model.Collider 

-- Returns whether a given tile type can be broken by the Player
isTileBreakable :: Tile -> Bool
isTileBreakable Crate             = True
isTileBreakable QuestionBlockFull = True
isTileBreakable _                 = False

-- Returns the tile at a given position
getTile :: World -> (Int, Int) -> Tile
getTile World { grid } (x, y) = (grid !! y) !! x

-- Sets a tile at a given position to a different tile type
setTile :: World -> (Int, Int) -> Tile -> World
setTile w@World {grid} (x, y) newTile = 
  let grid' = [if rIdx == y then setRow r else r | (r, rIdx) <- zip grid [0..]]
      setRow r = [if tIdx == x then newTile else t | (t, tIdx) <- zip r [0..]]
  in w { grid = grid', colliders = generateCollidersForWorld grid' }

-- Ensure the world's grid includes the given coordinate. If necessary, extend
-- with Air tiles on the appropriate sides. Returns the new world and the
-- adjusted coordinate within the new grid.
ensureInBounds :: World -> (Int, Int) -> (World, (Int, Int))
ensureInBounds w@World { grid = rows, slopes } (x, y) =
  let h = length rows
      w0 = if null rows then 0 else length (head rows)
      addLeft   = max 0 (-x)
      addTop    = max 0 (-y)
      addRight  = max 0 (x - (w0 - 1))
      addBottom = max 0 (y - (h - 1))
      newWidth  = addLeft + w0 + addRight
      -- Build new rows with padded columns
      padRow r = replicate addLeft Air ++ r ++ replicate addRight Air
      oldRowsPadded = map padRow rows
      airRow = replicate newWidth Air
      newRows = replicate addTop airRow ++ oldRowsPadded ++ replicate addBottom airRow
      newX = x + addLeft
      newY = y + addTop
      newColliders = generateCollidersForWorld newRows
  in (w { grid = newRows, colliders = newColliders, slopes = slopes }, (newX, newY))
