module Model.World where

import Model.Types
import Model.Collider 

-- Returns whether a given tile type can be broken by the Player
isTileBreakable :: Tile -> Bool
isTileBreakable Air                = False
isTileBreakable Grass              = False
isTileBreakable Crate              = True
isTileBreakable MetalBox           = False
isTileBreakable QuestionBlockFull  = False
isTileBreakable QuestionBlockEmpty = False

-- Returns the tile at a given position
getTile :: World -> (Int, Int) -> Tile
getTile World { grid } (x, y) = (grid !! y) !! x

-- Sets a tile at a given position to a different tile type
setTile :: World -> (Int, Int) -> Tile -> World
setTile w@World {grid} (x, y) newTile = 
  let grid' = [if rIdx == y then setRow r else r | (r, rIdx) <- zip grid [0..]]
      setRow r = [if tIdx == x then newTile else t | (t, tIdx) <- zip r [0..]]
  in w { grid = grid', colliders = generateCollidersForWorld grid' }
