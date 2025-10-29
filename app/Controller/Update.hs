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
update _ (Building bs)      = return (Building bs)

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
  . resolveInterEnemyOverlaps
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
      blockers = colliders (world gs) ++ blockingEntityColliders (entities gs)
      (jumpAccel, jumpTimer) = computeJumpHold dt gs p
      movedPlayer = applyMovement dt blockers gs jumpAccel p
      jumpTime'
        | onGround movedPlayer = jumpHoldDuration
              | otherwise            = jumpTimer
      jumpDir'
        | onGround movedPlayer = upVector
        | otherwise            = playerJumpDir movedPlayer
      -- Reset jump count when grounded; otherwise keep current
      jumpsAvailable
        | onGround movedPlayer = maxJumps
        | otherwise            = jumpsLeft movedPlayer
      -- Countdown stomp jump boost timer
      stompBoostTimeLeft = max 0 (stompJumpTimeLeft movedPlayer - dt)
      playerAfterHold = movedPlayer
        { playerJumpTime = jumpTime'
        , playerJumpDir  = jumpDir'
        , stompJumpTimeLeft = stompBoostTimeLeft
        }
      -- Allow jump if we have jumps left (double/triple jump)
      canJump = pendingJump gs && jumpsAvailable > 0
  in if canJump
        then
          let launchDir = computeJumpLaunchDir playerAfterHold
              impulseMag = if stompBoostTimeLeft > 0 then stompBoostedJumpImpulse else jumpImpulse
              impulse   = scaleVec launchDir impulseMag

              computeJumpLaunchDir :: Player -> Vector
              computeJumpLaunchDir Player { onGround, playerSlide, playerJumpDir } =
                normalizeVec $ case () of
                  _ | onGround -> upVector
                    | Just normal <- playerSlide -> addVec normal upVector
                    | otherwise -> playerJumpDir
          in 
            gs { player = 
              playerAfterHold
              { playerVel      =
                  let (vx, _) = playerVel playerAfterHold
                  in addVec (vx, 0) impulse
              , onGround       = False
              , playerJumpTime = 0
              , playerJumpDir  = launchDir
              , playerSlide    = Nothing
              , jumpsLeft      = jumpsAvailable - 1
              , stompJumpTimeLeft = if stompBoostTimeLeft > 0 then 0 else stompBoostTimeLeft
              }
            }
        else gs { player = playerAfterHold { jumpsLeft = jumpsAvailable } }

  where
    blockingEntityColliders :: [Entity] -> [Collider]
    blockingEntityColliders es = mapMaybe entityCollider (filter isBlocking es)
      where
        isBlocking :: Entity -> Bool
        isBlocking e = case e of
          EGoomba  _ _ -> True
          EKoopa   _ _ -> True
          EPlatform _   -> True
          EPowerup _ _ -> False


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
          -- Only block against world and player; enemy separation handled globally
          blockers        = colliders (world gs)
                             ++ maybeToList (playerCollider (player gs))
          collider        = specToCollider pos (CTEntity gId) spec
          (resolvedPos, flags, events) = resolveMovement collider pos displacement blockers
          velAfterCollision    = applyCollisionFlags flags velAfterAccel
          contactDrag          = contactFrictionAccel (contactNormals flags) velAfterCollision
          velWithFriction      = addVec velAfterCollision (scaleVec contactDrag dt)
          velLimited           = clampGoombaVelocity velWithFriction
          wallAheadProbe       =
            hitX flags &&
            let probeOffset   = (dirSign * wallProbeDistance, 0)
                probeCollider = specToCollider (addVecToPoint resolvedPos probeOffset) None spec
            in any (collides probeCollider) blockers
          hitWall              = wallAheadProbe
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

    -- Enemy-vs-enemy separation handled in resolveInterEnemyOverlaps
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

-- After entities update, push overlapping enemies (goomba/koopa) apart symmetrically
resolveInterEnemyOverlaps :: GameState -> GameState
resolveInterEnemyOverlaps gs@GameState { entities = es } =
  gs { entities = resolveAll es }
  where
    -- Do a couple of passes for stability
    resolveAll :: [Entity] -> [Entity]
    resolveAll = (!! 2) . iterate resolvePass

    resolvePass :: [Entity] -> [Entity]
    resolvePass ents = separatePairs 0 ents

    separatePairs :: Int -> [Entity] -> [Entity]
    separatePairs i ents
      | i >= length ents = ents
      | otherwise        = separateWith i (i + 1) ents

    separateWith :: Int -> Int -> [Entity] -> [Entity]
    separateWith i j ents
      | j >= length ents = separatePairs (i + 1) ents
      | otherwise        =
          case (ents !! i, ents !! j) of
            (e1, e2)
              | isEnemy e1 && isEnemy e2
              , Just c1 <- entityCollider e1
              , Just c2 <- entityCollider e2
              , Just (dx1, dy1, dx2, dy2) <- separation c1 c2 ->
                  let ents'  = updateAt i (moveBy (dx1, dy1)) ents
                      ents'' = updateAt j (moveBy (dx2, dy2)) ents'
                  in separateWith i (j + 1) ents''
              | otherwise -> separateWith i (j + 1) ents

    isEnemy :: Entity -> Bool
    isEnemy e = case e of
      EGoomba _ _ -> True
      EKoopa  _ _ -> True
      _           -> False

    moveBy :: Vector -> Entity -> Entity
    moveBy (dx, dy) e = case e of
      EGoomba eid g ->
        let newPos = addVecToPoint (goombaPos g) (dx, dy)
            newDir = if dx > 0 then Types.Right else if dx < 0 then Types.Left else goombaDir g
        in EGoomba eid g { goombaPos = newPos, goombaDir = newDir }
      EKoopa  eid k ->
        let newPos = addVecToPoint (koopaPos k) (dx, dy)
            newDir = if dx > 0 then Types.Right else if dx < 0 then Types.Left else koopaDir k
        in EKoopa  eid k { koopaPos  = newPos, koopaDir = newDir }
      _             -> e

    updateAt :: Int -> (Entity -> Entity) -> [Entity] -> [Entity]
    updateAt idx f xs =
      let (pre, rest) = splitAt idx xs
      in case rest of
          (y:ys) -> pre ++ f y : ys
          []     -> xs

    separation :: Collider -> Collider -> Maybe (Float, Float, Float, Float)
    separation (AABB (ax, ay) aw ah _) (AABB (bx, by) bw bh _)
      | overlapX > 0 && overlapY > 0 =
          if overlapX < overlapY
            then let sx = sign (ax - bx) * (overlapX / 2)
                 in Just ( sx, 0, -sx, 0 )
            else let sy = sign (ay - by) * (overlapY / 2)
                 in Just ( 0,  sy,  0, -sy )
      | otherwise = Nothing
      where
        overlapX = (aw + bw) / 2 - abs (ax - bx)
        overlapY = (ah + bh) / 2 - abs (ay - by)
        sign v = if v >= 0 then 1 else -1
