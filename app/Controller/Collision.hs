module Controller.Collision where

import Model.Types
import qualified Model.Types as Types
import Model.World
import Model.Entity
import Data.List (foldl')
import Model.Collider
import Model.Config (stompBounceVelocity, stompJumpWindow)


handleCollisionEvents :: GameState -> GameState
handleCollisionEvents gs =
  let gs'   = handlePlayerCollisions gs
      gs''  = handlePowerupCollisions gs'
      gs''' = handleEnemyCollisions gs''
  in gs'''

handlePlayerCollisions :: GameState-> GameState
handlePlayerCollisions gameState@GameState { player } =
  foldl handleEvent gameState (playerCollisions player)
  -- Process each event individually
  where
    handleEvent gs@GameState {world = w} CollisionEvent { colEventTag, colEventAxis, colEventNormal }=
      case colEventTag of
        CTEntity eId ->
          case getEntity gs eId of
          --case trace ("Collided with entity (id: " ++ show eId ++ ")") getEntity gs eId of
            Nothing -> gs
            Just entity -> case entity of
              (EGoomba _ _) ->
                if colEventAxis == AxisY && snd colEventNormal > 0 -- check if player jumped on top of goomba
                  then
                    -- Player touched goomba from top: kill goomba and bounce player up
                    let gsKilled = killEntity gs eId
                        p0 = Types.player gsKilled
                        (vx, _) = playerVel p0
                        p' = p0 { playerVel = (vx, stompBounceVelocity), onGround = False, stompJumpTimeLeft = stompJumpWindow }
                    in gsKilled { player = p' }
                  else
                    -- Player touched goomba from side/bottom, take damage
                    let 
                      removeEntity gs' = killEntity gs' eId
                    in removeEntity $ damagePlayer gs
              (EPowerup _ _) -> 
                healPlayer $ killEntity gs eId
              _            -> gs
        CTWorld  coords@(tX, tY)       ->
          -- trace ("Tile at (" ++ show x ++ ", " ++ show y ++ ")")
          let
            hitTile = getTile w coords
          in if colEventAxis == AxisY && snd colEventNormal < 0 -- check if collision was on bottom side of tile
              then
                -- Find tile hit, and destroy if breakable
                if isTileBreakable hitTile
                  then case hitTile of
                    QuestionBlockFull ->
                      let 
                        w' = setTile w coords QuestionBlockEmpty
                      --w' = setTile w coords $ trace ("Pos: " ++ show coords) QuestionBlockEmpty
                        newPowerup = EPowerup 0 defaultPowerup { powerupPos = (fromIntegral tX + 0.5, fromIntegral (-tY) + 0.5) }
                        gsAfterSpawn = spawnEntity gs newPowerup
                      in gsAfterSpawn { world = w' }
                    _ -> gs { world = setTile w coords Air }
                  else gs
              else case hitTile of
                Flag -> gs { nextState = NFinishLevel }
                _    -> gs
              
        _ -> gs

-- Handle entity-side collision outcomes that the player handler doesn't catch
-- Specifically: allow powerups to be collected when they move into the player
handlePowerupCollisions :: GameState -> GameState
handlePowerupCollisions gs@GameState { entities, player = p } =
  foldl' step gs entities
  where
    step acc e = case e of
      EPowerup eId pu ->
        let touchedPlayerEvent = any isPlayerEvent (powerupCollisions pu)
            overlapWithPlayer  = overlapsPlayer acc eId pu
            shouldCollect      = touchedPlayerEvent || overlapWithPlayer
        in if shouldCollect
            then case getEntity acc eId of
                  Nothing -> acc
                  Just _  -> healPlayer (killEntity acc eId)
            else acc
      _ -> acc

    isPlayerEvent :: CollisionEvent -> Bool
    isPlayerEvent CollisionEvent { colEventTag } = case colEventTag of
      CTPlayer _ -> True
      _          -> False

    overlapsPlayer :: GameState -> Int -> Powerup -> Bool
    overlapsPlayer st puId pu =
      case (playerCollider (player st), powerupCollider puId pu) of
        (Just pc, Just ec) -> collides pc ec
        _                  -> False

-- Allow enemies to affect the player even if only the enemy moved
-- Logic mirrors player-side handling:
--  - If enemy hits player from below (player above): kill enemy (stomp)
--  - Otherwise: damage player and remove the enemy
handleEnemyCollisions :: GameState -> GameState
handleEnemyCollisions gs@GameState { entities } =
  foldl' step gs entities
  where
    step acc e = case e of
      EGoomba eId g ->
        resolveEnemyVsPlayer acc eId (goombaCollisions g)
      EKoopa eId k ->
        resolveEnemyVsPlayer acc eId (koopaCollisions k)
      _ -> acc

    resolveEnemyVsPlayer :: GameState -> Int -> [CollisionEvent] -> GameState
    resolveEnemyVsPlayer acc eId evs =
      case findPlayerEvent evs of
        Nothing -> acc
        Just ev ->
          let stomp = isStompOnEnemy ev
          in case getEntity acc eId of
              Nothing -> acc
              Just _  -> if stomp
                          then let accBounce =
                                       let p0 = Types.player acc
                                           (vx, _) = playerVel p0
                                           p' = p0 { playerVel = (vx, stompBounceVelocity), onGround = False, stompJumpTimeLeft = stompJumpWindow }
                                       in acc { player = p' }
                               in killEntity accBounce eId
                          else let acc' = damagePlayer acc
                               in killEntity acc' eId

    findPlayerEvent :: [CollisionEvent] -> Maybe CollisionEvent
    findPlayerEvent = foldl' (\m ev -> case m of { Just _ -> m; Nothing -> if touchesPlayer ev then Just ev else Nothing }) Nothing

    touchesPlayer :: CollisionEvent -> Bool
    touchesPlayer CollisionEvent { colEventTag } = case colEventTag of
      CTPlayer _ -> True
      _          -> False

    -- From enemy's perspective: AxisY with normal.y < 0 means the player is above
    isStompOnEnemy :: CollisionEvent -> Bool
    isStompOnEnemy CollisionEvent { colEventAxis, colEventNormal = (_, ny) } =
      colEventAxis == AxisY && ny < 0
