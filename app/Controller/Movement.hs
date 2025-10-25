module Controller.Movement where

import Graphics.Gloss
import Data.List (nub, find)
import Data.Maybe (maybeToList)

import Model.Types
import Model.Config
import MathUtils
import Model.Collider

-- Calulates accelleration based on gravity, input, drag
-- Moves the player
--  If player has a collider: resolve movement (via resolveMovement function)
--  Otherwise: Simply do position += velocity
-- Based on collision flags and friction, update velocity
applyMovement :: Float -> [Collider] -> GameState -> Vector -> Player -> Player
applyMovement dt blockers gs jumpAccel p@Player 
  { playerPos = pos
  , playerVel = vel
  , playerColliderSpec
  , playerSlide = prevSlide
  } =
  let -- Calulate desired acceleration
    gravityAccel = (0, gravityAcceleration)
    moveDirRaw = boolToDir (moveLeftHeld gs) (moveRightHeld gs)
    boolToDir left right = (if right then 1.0 else 0.0) - (if left then 1.0 else 0.0)
    moveDir = clampInput moveDirRaw
    controlAccel =
      if moveDir == 0
        then (0, 0)
        else
          let controlMag
                | onGround p =
                    let baseAccel = if sprintHeld gs then groundSprintAccel else groundWalkAccel
                        vx = fst vel
                        sameDir = moveDir * vx >= 0
                    in if sameDir then baseAccel else groundMoveDecel
                | otherwise =
                    let baseAccel = if sprintHeld gs then airSprintAccel else airWalkAccel
                        vx = fst vel
                        sameDir = moveDir * vx >= 0
                    in if sameDir then baseAccel else airMoveDecel
          in (moveDir * controlMag, 0)
    airDrag = (- (airFrictionCoeff * fst vel), 0)
    totalAccel = gravityAccel
                  `addVec` controlAccel
                  `addVec` airDrag
                  `addVec` jumpAccel
    velAfterAccel = addVec vel (scaleVec totalAccel dt)
    displacement = scaleVec velAfterAccel dt -- Desired movement for this timestep
    -- If player has a collider, resolve movement to prevent collider overlap
    -- else, simply move the player along the desired displacement
    (resolvedPos, flags, events) =
      case playerColliderSpec of
        Nothing   -> (addVecToPoint pos displacement, noCollisionFlags, [])
        Just spec -> 
          let collider = specToCollider pos (CTPlayer p) spec
          in resolveMovement collider pos displacement blockers
    -- Update velocity based on collision flags
    velAfterCollision = applyCollisionFlags flags velAfterAccel
    contacts = contactNormals flags
    newSlide = determineSlide resolvedPos contacts
    frictionContacts = contacts ++ maybeToList newSlide
    vyAfterCollision = snd velAfterCollision
    -- Calculate drag and finalize new velocity
    contactDrag
      | vyAfterCollision > 0 = (0, 0)
      | otherwise            = contactFrictionAccel frictionContacts velAfterCollision
    velFinal = addVec velAfterCollision (scaleVec contactDrag dt)
    onGround' = groundContact flags
  in p { playerPos = resolvedPos
       , playerVel = velFinal
       , onGround = onGround'
       , playerSlide = newSlide
       , playerCollisions = events
       }
  where 
    clampInput :: Float -> Float
    clampInput x
      | x > 1     = 1
      | x < -1    = -1
      | otherwise = x
    
    determineSlide :: Point -> [Vector] -> Maybe Vector
    determineSlide pos' contacts =
      case find isWallNormal contacts of
        Just wall -> Just wall
        Nothing   ->
          case (playerColliderSpec, prevSlide) of
            (Just s, Just normal)
              | isWallNormal normal
              , stillTouching s normal -> Just normal
            _ -> Nothing
      where
        isWallNormal :: Vector -> Bool
        isWallNormal (nx, ny) = abs nx > 0.5 && abs ny < 0.75

        stillTouching :: ColliderSpec -> Vector -> Bool
        stillTouching s normal =
          let probePos = addVecToPoint pos' (scaleVec (negateVec normal) slideProbeDistance)
              testCollider = specToCollider probePos None s
          in any (collides testCollider) blockers

-- Attempts to move a colider along a displacement vector
-- Recursively sweeps the path to find precise collision point (if any)
resolveMovement :: Collider -> Point -> Vector -> [Collider]
                -> (Point, CollisionFlags, [CollisionEvent])
resolveMovement collider start displacement blockers =
  go 3 start displacement noCollisionFlags []
  where
    go :: Int -> Point -> Vector -> CollisionFlags -> [CollisionEvent] -> (Point, CollisionFlags, [CollisionEvent])
    go 0 pos _ flags events = (pos, flags, events)
    go _ pos disp flags events | nearZeroVec disp = (pos, flags, events) -- if remaining displacement is near zero, end recursion
    go attempts pos disp flags events =
      let endPos = addVecToPoint pos disp
          sweep = sweepSegment pos disp 5
      in case sweep of
           SweepClear finalPos -> (finalPos, flags, events)
           SweepHit { safePos = safe, hitPos, hitBlocker = blocker } ->
             let (normal, axis) = collisionNormal collider { aPos = hitPos} blocker
                 remainder = subPoints endPos safe
                 remainingAfterCollision = removeNormalComponent remainder axis
                 flagsThisCollision = collisionFlagsFor axis normal
                 combinedFlags = combineFlags flags flagsThisCollision
                 newEvent = CollisionEvent {
                  colEventTag    = tag blocker,
                  colEventAxis   = axis,
                  colEventNormal = normal
                 }
                 --newEvents = trace (show collider ++ " collided with " ++ show blocker) events ++ [newEvent]
                 newEvents = events ++ [newEvent]
                 
             in if nearZeroVec remainingAfterCollision
                  then (safe, combinedFlags, newEvents)
                  else go (attempts - 1) safe remainingAfterCollision combinedFlags newEvents

    -- Checks if a collision occurs along a displacement path
    sweepSegment :: Point -> Vector -> Int -> SweepResult
    sweepSegment pos disp steps = sweep 1 pos
      where
        sweep i prevPos
          | i > steps = SweepClear (addVecToPoint pos disp)
          | otherwise =
              let t = fromIntegral i / fromIntegral steps
                  current = addVecToPoint pos (scaleVec disp t)
                  hit = find (collides collider {aPos = current }) blockers
              in case hit of
                  Just blocker -> SweepHit { safePos = prevPos, hitPos = current, hitBlocker = blocker }
                  Nothing      -> sweep (i + 1) current

    -- Helps to avoid super small displacements
    nearZeroVec :: Vector -> Bool
    nearZeroVec (x, y) = abs x < 0e-4 && abs y < 1e-4

    removeNormalComponent :: Vector -> Axis -> Vector
    removeNormalComponent (_, dy) AxisX = (0, dy)
    removeNormalComponent (dx, _) AxisY = (dx, 0)

    collisionFlagsFor :: Axis -> Vector -> CollisionFlags
    collisionFlagsFor axis normal =
      let ground = isGroundNormal normal
      in case axis of
          AxisX -> CollisionFlags True False ground [normal]
          AxisY -> CollisionFlags False True ground [normal]

    isGroundNormal :: Vector -> Bool
    isGroundNormal (nx, ny) =
      ny > 0 && ny >= abs nx

    collisionNormal :: Collider -> Collider -> (Vector, Axis)
    collisionNormal (AABB (ex, ey) ew eh _) (AABB (bx, by) bw bh _) =
      let dx = ex - bx
          dy = ey - by
          overlapX = (ew + bw) / 2 - abs dx
          overlapY = (eh + bh) / 2 - abs dy
          axis
            | overlapX < overlapY = AxisX
            | otherwise            = AxisY
          dirX = if dx >= 0 then 1 else -1
          dirY = if dy >= 0 then 1 else -1
          normal = case axis of
            AxisX -> (dirX, 0)
            AxisY -> (0, dirY)
      in (normal, axis)

-- Used in applyMovement (for player) and updateGoomba
contactFrictionAccel :: [Vector] -> Vector -> Vector
contactFrictionAccel normals vel =
  foldl addVec (0, 0)
    [ scaleVec tangent (- (groundFrictionCoeff * dotVec vel tangent))
    | tangent <- map tangentFromNormal (unique normals)
    ]
  where
    unique = nub

-- We gebruiken op dit moment niet echt type class abstractions
-- is wel handig om later nog toe te passen want vgm vinden ze dat mooier/beter
-- voor als we n beter cijfertje willen

-- class Movable e where
--   getPos :: e -> Point
--   setPos :: e -> Point -> e
--   getVel :: e -> Vector
--   setVel :: e -> Vector -> e

-- class Movable e => Rigidbody e where
--   getLevelCollisions :: e -> TileMap -> [TileMap]

-- updateMovable :: Movable e => Float -> e -> e
-- updateMovable dt e =
--   let (vx, vy)   = getVel e
--       (x,  y)    = getPos e
--    in setPos e (x + vx * dt, y + vy * dt)

-- updatePhysics :: Rigidbody e => Float -> e -> e
-- updatePhysics dt e =
--   let (vx, vy) = getVel e
--   in setVel e (vx, vy + gravityAcceleration * dt)



-- instance Movable Player where
--   getPos       = playerPos
--   setPos p pos = p { playerPos = pos }
--   getVel       = playerVel
--   setVel p vel = p { playerVel = vel }

-- instance Rigidbody Player where
--   getLevelCollisions = undefined

-- instance Movable Goomba where
--   getPos       = goombaPos
--   setPos g pos = g { goombaPos = pos }
--   getVel       = goombaVel
--   setVel g vel = g { goombaVel = vel }

-- instance Rigidbody Goomba where
--   getLevelCollisions = undefined:w
