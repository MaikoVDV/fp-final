module Controller.Collision where

import Model.Types
import Model.Collider

setTile :: World -> (Int, Int) -> Tile -> World
setTile w@World {grid} (x, y) newTile = 
  let grid' = [if rIdx == y then setRow r else r | (r, rIdx) <- zip grid [0..]]
      setRow r = [if tIdx == x then newTile else t | (t, tIdx) <- zip r [0..]]
  in w { grid = grid', colliders = generateCollidersForWorld grid' }


handleCollisionEvents :: GameState -> GameState
handleCollisionEvents gs =
  let gs' = handlePlayerCollisions gs
  in gs'

handlePlayerCollisions :: GameState-> GameState
handlePlayerCollisions gameState@GameState { player } =
  foldl handleEvent gameState (playerCollisions player)
  -- Process each event individually
  where
    handleEvent gs@GameState {player = p, entities = es, world = w} CollisionEvent { colEventTag, colEventAxis, colEventNormal }=
      case colEventTag of
        CTEntity (EGoomba gId _) -> 
          if colEventAxis == AxisY && snd colEventNormal > 0 -- check if player jumped on top of goomba
            then 
              -- Player touched goomba from top, kill goomba
              let es' = filter (\e -> case e of
                    (EGoomba xId _) -> xId /= gId
                    _ -> True
                    ) es
              in gs {player = p, entities = es'}
            else
              -- Player touched goomba from side/bottom, take damage
              gs {player = p {playerPos = (0, 0)}, entities = es}
        CTEntity (EKoopa _ _) -> gs
        CTWorld  coords       ->
          -- trace ("Tile at (" ++ show x ++ ", " ++ show y ++ ")")
          if colEventAxis == AxisY && snd colEventNormal < 0 -- check if collision was on bottom side of tile
            then 
              -- Find tile hit, and destroy
              let w' = setTile w coords Air
              in gs { world = w' }
            else
              gs
        _ -> gs