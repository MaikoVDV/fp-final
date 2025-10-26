module Controller.Collision where

import Model.Types
import Model.World
import Model.Entity
import Debug.Trace


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
        CTEntity eId ->
          case getEntity gs eId of
          --case trace ("Collided with entity (id: " ++ show eId ++ ")") getEntity gs eId of
            Nothing -> gs
            Just entity -> case entity of
              (EGoomba _ _) ->
                if colEventAxis == AxisY && snd colEventNormal > 0 -- check if player jumped on top of goomba
                  then
                    -- Player touched goomba from top, kill goomba
                    killEntity gs eId
                  else
                    -- Player touched goomba from side/bottom, take damage
                    gs {player = p {playerPos = (0, 0)}, entities = es}
              (EPowerup _ _) -> trace "Collected a powerup!" killEntity gs eId
              _            -> gs
        CTWorld  coords@(tX, tY)       ->
          -- trace ("Tile at (" ++ show x ++ ", " ++ show y ++ ")")
          if colEventAxis == AxisY && snd colEventNormal < 0 -- check if collision was on bottom side of tile
            then
              let
                hitTile = getTile w coords
                -- Find tile hit, and destroy if breakable
              in if isTileBreakable hitTile
                  then gs { world = setTile w coords Air }
                  -- Handle special behavior of some tiles
                  else case hitTile of
                    QuestionBlockFull ->
                      let w' = setTile w coords QuestionBlockEmpty
                      --let w' = setTile w coords $ trace ("Pos: " ++ show coords) QuestionBlockEmpty
                          gsAfterSpawn = spawnEntity gs (EPowerup 0 defaultPowerup { powerupPos = (fromIntegral tX + 0.5, fromIntegral (-tY) + 0.5) } )
                      in gsAfterSpawn { world = w' }
                    _             -> gs
            else
              gs
        _ -> gs