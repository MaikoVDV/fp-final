module Controller.Collision where

import Model.Types
import qualified Model.Types as Types
import Model.World
import Model.Entity
import Data.List (foldl')
import Model.Collider
import Model.Config (stompBounceVelocity, stompJumpWindow, goombaShellDuration, goombaContactDamage)


handleCollisionEvents :: GameState -> GameState
handleCollisionEvents gs =
  let gs'   = handlePlayerCollisions gs
      gs''  = handleCollectibleCollisions gs'
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
              (EGoomba _ g) ->
                if colEventAxis == AxisY && snd colEventNormal > 0 -- player came from above
                  then
                    let p0 = Types.player gs
                        (vx, _) = playerVel p0
                        p' = p0 { playerVel = (vx, stompBounceVelocity), onGround = False, stompJumpTimeLeft = stompJumpWindow }
                    in case goombaMode g of
                        GWalking ->
                          -- First stomp: enter shell mode and stop moving
                          let gsShelled = updateGoombaById gs eId (\go -> go { goombaMode = GShelled goombaShellDuration
                                                                            , goombaVel = (0, snd (goombaVel go)) })
                          in gsShelled { player = p' }
                        GShelled _ ->
                          -- Second stomp while shelled: kill it
                          let gsKilled = killEntity gs eId
                          in gsKilled { player = p' }
                  else
                    -- Side/bottom touch: damage player (if not invulnerable)
                    damagePlayerN goombaContactDamage gs
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
handleCollectibleCollisions :: GameState -> GameState
handleCollectibleCollisions gs@GameState { entities, player = p } =
  foldl' step gs entities
  where
    step acc e = case e of
      EPowerup eId pu ->
        let touchedPlayerEvent = any isPlayerEvent (powerupCollisions pu)
            overlapWithPlayer  = overlapsPowerupPlayer acc eId pu
            shouldCollect      = touchedPlayerEvent || overlapWithPlayer
        in if shouldCollect
            then case getEntity acc eId of
                  Nothing -> acc
                  Just _  -> healPlayer (killEntity acc eId)
            else acc
      ECoin eId c ->
        let overlap = overlapsCoinPlayer acc eId c
        in if overlap
            then case getEntity acc eId of
                  Nothing -> acc
                  Just _  -> addCoins 1 (killEntity acc eId)
            else acc
      _ -> acc

    isPlayerEvent :: CollisionEvent -> Bool
    isPlayerEvent CollisionEvent { colEventTag } = case colEventTag of
      CTPlayer _ -> True
      _          -> False

    overlapsPowerupPlayer :: GameState -> Int -> Powerup -> Bool
    overlapsPowerupPlayer st puId pu =
      case (playerCollider (player st), powerupCollider puId pu) of
        (Just pc, Just ec) -> collides pc ec
        _                  -> False

    overlapsCoinPlayer :: GameState -> Int -> Coin -> Bool
    overlapsCoinPlayer st cId c =
      case (playerCollider (player st), coinCollider cId c) of
        (Just pc, Just ec) -> collides pc ec
        _                  -> False

-- Allow enemies to affect the player even if only the enemy moved
-- Logic mirrors player-side handling:
--  - If enemy hits player from below (player above): kill enemy (stomp)
--  - Otherwise: damage player and remove the enemy
handleEnemyCollisions :: GameState -> GameState
handleEnemyCollisions gs@GameState { entities } =
  case playerCollider (player gs) of
    Nothing -> gs
    Just pc -> foldl' (overlapStep pc) gs entities
  where
    overlapStep :: Collider -> GameState -> Entity -> GameState
    overlapStep pc acc e = case e of
      EGoomba eId g ->
        case (goombaCollider eId g) of
          Just ec | collides pc ec ->
            let stomp = isStomp acc g in applyGoombaHit acc eId g stomp
          _ -> acc
      EKoopa eId k ->
        case (koopaCollider eId k) of
          Just ec | collides pc ec -> damagePlayerN goombaContactDamage acc
          _ -> acc
      _ -> acc

    isStomp :: GameState -> Goomba -> Bool
    isStomp st g =
      let (_, py) = playerPos (player st)
          (_, gy) = goombaPos g
          (_, pvy) = playerVel (player st)
      in py > gy && pvy < 0

    applyGoombaHit :: GameState -> Int -> Goomba -> Bool -> GameState
    applyGoombaHit acc eId g stomp =
      case getEntity acc eId of
        Nothing -> acc
        Just (EGoomba _ _) ->
          if stomp
            then
              let p0 = Types.player acc
                  (vx, _) = playerVel p0
                  p' = p0 { playerVel = (vx, stompBounceVelocity), onGround = False, stompJumpTimeLeft = stompJumpWindow }
              in case goombaMode g of
                   GWalking ->
                     let accShelled = updateGoombaById acc eId (\go -> go { goombaMode = GShelled goombaShellDuration
                                                                         , goombaVel = (0, snd (goombaVel go)) })
                     in accShelled { player = p' }
                   GShelled _ ->
                     let accBounce = acc { player = p' }
                     in killEntity accBounce eId
            else damagePlayerN goombaContactDamage acc
        _ -> acc
