module Controller.Update where

import Graphics.Gloss
import Data.Maybe (mapMaybe, maybeToList, isJust)

import Model.Types
import qualified Model.Types as Types
import Model.Collider
import Model.Config

import Controller.Input
import Controller.Collision
import Controller.Movement

import MathUtils

update :: Float -> AppState -> IO AppState
update _  menuState@(Menu _) = return menuState
update dt (Playing gs)       =
  let gs' = updateGame dt gs
  in case nextState gs' of
    NPlaying -> return . Playing $ gs'
    _       -> return (Menu $ menuState gs')

-- updateGame :: Float -> GameState -> GameState
-- updateGame dt gs =
--   let 
--     updatedPlayer   = updatePlayer dt gs
--     updatedEntities = map (updateEntity dt gs) (entities gs)
--     updatedState = gs
--       { player = updatedPlayer
--       , entities = updatedEntities
--       , pendingJump = False
--       }
--     finalState = handleCollisionEvents updatedState
--   in finalState { frameCount = frameCount gs + 1 }

updateGame :: Float -> GameState -> GameState
updateGame dt =
  handleCollisionEvents
  . updateEntities dt
  . clearJump
  . updatePlayer dt
  . incrementFrame
  where 
    incrementFrame :: GameState -> GameState
    incrementFrame gs@GameState { frameCount }
      = gs {frameCount = frameCount + 1, nextState = NPlaying}

    clearJump ::GameState -> GameState
    clearJump gs = gs { pendingJump = False }


updatePlayer :: Float -> GameState -> GameState
updatePlayer dt gs =
  let p = player gs
      blockers = colliders (world gs) ++ mapMaybe entityCollider (entities gs)
      (jumpAccel, jumpTimer) = computeJumpHold dt gs p
      movedPlayer = applyMovement dt blockers gs jumpAccel p
      jumpTime'
        | onGround movedPlayer = jumpHoldDuration
              | otherwise            = jumpTimer
      jumpDir'
        | onGround movedPlayer = upVector
        | otherwise            = playerJumpDir movedPlayer
      playerAfterHold = movedPlayer
        { playerJumpTime = jumpTime'
        , playerJumpDir  = jumpDir'
        }
      canJump = pendingJump gs && (onGround playerAfterHold || isJust (playerSlide playerAfterHold))
  in if canJump
        then
          let launchDir = computeJumpLaunchDir playerAfterHold
              impulse   = scaleVec launchDir jumpImpulse

              computeJumpLaunchDir :: Player -> Vector
              computeJumpLaunchDir Player { onGround, playerSlide, playerJumpDir } =
                normalizeVec $ case () of
                  _ | onGround -> upVector
                    | Just normal <- playerSlide -> addVec normal upVector
                    | otherwise -> playerJumpDir
          in 
            gs { player = 
              playerAfterHold
              { playerVel      = addVec (playerVel playerAfterHold) impulse
              , onGround       = False
              , playerJumpTime = 0
              , playerJumpDir  = launchDir
              , playerSlide    = Nothing
              }
            }
        else gs { player = playerAfterHold }


updateEntities :: Float -> GameState -> GameState
updateEntities dt gs = gs { entities = map (updateEntity dt gs) (entities gs) }
-- Handle updating different types of entities separately
updateEntity :: Float -> GameState -> Entity -> Entity
updateEntity dt gs (EGoomba  gId  g)  = EGoomba  gId  (updateGoomba  dt gs gId  g)
updateEntity _  _  (EKoopa   kId  k)  = EKoopa   kId  k
updateEntity dt gs (EPowerup puId pu) = EPowerup puId (updatePowerup dt gs puId pu)
updateEntity _  _  e             = e

updateGoomba :: Float -> GameState -> Int -> Goomba -> Goomba
updateGoomba dt gs gId g@Goomba
  { goombaPos = pos
  , goombaVel = vel
  , goombaColSpec = colSpec
  , goombaDir = dir
  } =
  case colSpec of
    Nothing ->
      let velAfterAccel = addVec vel (scaleVec totalAccel dt)
          velLimited    = clampGoombaVelocity velAfterAccel
          newPos        = addVecToPoint pos (scaleVec velLimited dt)
      in g { goombaPos = newPos
           , goombaVel = velLimited
           }
    Just spec ->
      let velAfterAccel   = addVec vel (scaleVec totalAccel dt)
          displacement    = scaleVec velAfterAccel dt
          enemyBlockers    = otherEnemyColliders gId (entities gs)
          blockers        = colliders (world gs)
                             ++ maybeToList (playerCollider (player gs))
                             ++ enemyBlockers
          collider        = specToCollider pos (CTEntity gId) spec
          (resolvedPos0, flags, events) = resolveMovement collider pos displacement blockers
          -- If this goomba touched another enemy, revert to original position to avoid overlap
          touchedEnemy     = any (collidedWithOtherEnemy gId (entities gs)) events
          resolvedPos      = if touchedEnemy then pos else resolvedPos0
          velAfterCollision    = applyCollisionFlags flags velAfterAccel
          contactDrag          = contactFrictionAccel (contactNormals flags) velAfterCollision
          velWithFriction      = addVec velAfterCollision (scaleVec contactDrag dt)
          velLimited           = clampGoombaVelocity velWithFriction
          wallAheadProbe       =
            hitX flags &&
            let probeOffset   = (dirSign * wallProbeDistance, 0)
                probeCollider = specToCollider (addVecToPoint resolvedPos probeOffset) None spec
            in any (collides probeCollider) blockers
          hitWall              = wallAheadProbe || touchedEnemy
          newDirBase           = if hitWall then flipDir dir else dir
          newDir               = newDirBase
          adjVx                = if hitWall then 0 else fst velLimited
          velFinal             = (adjVx, snd velLimited)
      in g { goombaPos        = resolvedPos
           , goombaVel        = velFinal
           , goombaDir        = newDir
           , goombaOnGround   = groundContact flags
           , goombaCollisions = events
           }
  where
    gravityAccel  = (0, gravityAcceleration)
    dirSign       = case dir of { Types.Left -> -1.0; Types.Right -> 1.0 }
    goombaAccel   = (dirSign * goombaMoveAccel, 0)
    airDrag       = (- (airFrictionCoeff * fst vel), 0)
    totalAccel    = gravityAccel `addVec` goombaAccel `addVec` airDrag
    clampGoombaVelocity (vx, vy) =
      let vx' = max (-goombaWalkSpeed) (min goombaWalkSpeed vx)
      in (vx', vy)
    flipDir Types.Left  = Types.Right
    flipDir Types.Right = Types.Left

    -- Colliders for other enemies (prevent inter-enemy overlap)
    otherEnemyColliders :: Int -> [Entity] -> [Collider]
    otherEnemyColliders selfId es =
      let isOtherEnemy e = case e of
            EGoomba  eid _ -> eid /= selfId
            EKoopa   eid _ -> eid /= selfId
            _              -> False
          toCol e = entityCollider e
      in mapMaybe toCol (filter isOtherEnemy es)

    -- Detect if a collision event was with another enemy (goomba/koopa)
    collidedWithOtherEnemy :: Int -> [Entity] -> CollisionEvent -> Bool
    collidedWithOtherEnemy selfId es CollisionEvent { colEventTag } = case colEventTag of
      CTEntity eid ->
        eid /= selfId && any (\e -> case e of
          EGoomba  id' _ -> id' == eid
          EKoopa   id' _ -> id' == eid
          _              -> False) es
      _ -> False
updatePowerup :: Float -> GameState -> Int -> Powerup -> Powerup
updatePowerup dt gs puId pu@Powerup
  { powerupPos = pos
  , powerupVel = vel
  , powerupColSpec = colSpec
  , powerupDir = dir
  } =
  case colSpec of
    Nothing ->
      let velAfterAccel = addVec vel (scaleVec totalAccel dt)
          velLimited    = clampGoombaVelocity velAfterAccel
          newPos        = addVecToPoint pos (scaleVec velLimited dt)
      in pu { powerupPos = newPos
            , powerupVel = velLimited
            }
    Just spec ->
      let velAfterAccel   = addVec vel (scaleVec totalAccel dt)
          displacement    = scaleVec velAfterAccel dt
          blockers        = colliders (world gs) ++ maybeToList (playerCollider (player gs))
          collider        = specToCollider pos (CTEntity puId) spec
          (resolvedPos, flags, events) = resolveMovement collider pos displacement blockers
          velAfterCollision    = applyCollisionFlags flags velAfterAccel
          contactDrag          = contactFrictionAccel (contactNormals flags) velAfterCollision
          velWithFriction      = addVec velAfterCollision (scaleVec contactDrag dt)
          velLimited           = clampGoombaVelocity velWithFriction
          wallAhead            =
            hitX flags &&
            let probeOffset   = (dirSign * wallProbeDistance, 0)
                probeCollider = specToCollider (addVecToPoint resolvedPos probeOffset) None spec
            in any (collides probeCollider) blockers
          hitWall              = wallAhead
          newDirBase           = if hitWall then flipDir dir else dir
          newDir               =
            if hitWall
              then newDirBase
              --then trace ("[DEBUG] Powerup (id: " ++ show puId ++ ") wall collision at " ++ show resolvedPos ++ "\n") newDirBase
              else newDirBase
          adjVx                = if hitWall then 0 else fst velLimited
          velFinal             = (adjVx, snd velLimited)
      in pu { powerupPos        = resolvedPos
            , powerupVel        = velFinal
            , powerupDir        = newDir
            , powerupCollisions = events
            }
  where
    gravityAccel  = (0, gravityAcceleration)
    dirSign       = case dir of { Types.Left -> -1.0; Types.Right -> 1.0 }
    goombaAccel   = (dirSign * goombaMoveAccel, 0)
    airDrag       = (- (airFrictionCoeff * fst vel), 0)
    totalAccel    = gravityAccel `addVec` goombaAccel `addVec` airDrag
    clampGoombaVelocity (vx, vy) =
      let vx' = max (-goombaWalkSpeed) (min goombaWalkSpeed vx)
      in (vx', vy)
    flipDir Types.Left  = Types.Right
    flipDir Types.Right = Types.Left
