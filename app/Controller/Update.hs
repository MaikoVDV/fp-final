module Controller.Update where

import Graphics.Gloss
import Data.Maybe (mapMaybe, maybeToList, isJust)
import Debug.Trace

import Model.Types
import qualified Model.Types as Types
import Model.Collider
import Model.Config

import Controller.Input
import Controller.Collision
import Controller.Movement

import MathUtils

update :: Float -> AppState -> IO AppState
update _ menuState@(Menu _) = return menuState
update dt (Playing gs) = return . Playing $ updateGame dt gs

updateGame :: Float -> GameState -> GameState
updateGame dt gs =
  let updatedPlayer   = updatePlayer dt gs
      updatedEntities = map (updateEntity dt gs) (entities gs)
      updatedState = gs { player = updatedPlayer
        , entities = updatedEntities
        , pendingJump = False
        }
      finalState = handleCollisionEvents updatedState
  in finalState

updatePlayer :: Float -> GameState -> Player
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
          in playerAfterHold
               { playerVel      = addVec (playerVel playerAfterHold) impulse
               , onGround       = False
               , playerJumpTime = 0
               , playerJumpDir  = launchDir
               , playerSlide    = Nothing
               }
        else playerAfterHold


-- Handle updating different types of entities separately
updateEntity :: Float -> GameState -> Entity -> Entity
updateEntity dt gs (EGoomba gId g) = EGoomba gId (updateGoomba dt gs gId g)
updateEntity _  _  (EKoopa  kId k) = EKoopa  kId k
updateEntity _  _  e             = e

updateGoomba :: Float -> GameState -> Int -> Goomba -> Goomba
updateGoomba dt gs gId g@Goomba { goombaPos = pos
                            , goombaVel = vel
                            , goombaColliderSpec = mSpec
                            , goombaDir = dir
                            } =
  case mSpec of
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
          blockers        = colliders (world gs) ++ maybeToList (playerCollider (player gs))
          collider        = specToCollider pos (CTEntity (EGoomba gId g)) spec
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
              then trace ("[DEBUG] Goomba wall collision at " ++ show resolvedPos ++ "\n") newDirBase
              else newDirBase
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