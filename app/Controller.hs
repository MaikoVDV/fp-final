module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (mapMaybe)
import qualified Collision
import Data.List (find)

gravityAcceleration :: Float
gravityAcceleration = -25

jumpImpulse :: Float
jumpImpulse = 8

jumpStartAcceleration :: Float
jumpStartAcceleration = 30

jumpAccelerationTime :: Float
jumpAccelerationTime = 0.7

groundWalkAccel, groundSprintAccel, airMoveAccel, airMoveDecel :: Float
groundWalkAccel = 60
groundSprintAccel = 90
airMoveAccel = 18
airMoveDecel = 24

groundFrictionCoeff, airFrictionCoeff :: Float
groundFrictionCoeff = 10
airFrictionCoeff = 1.5

input :: Event -> GameState -> IO GameState
input e gs = return $ handleInput e gs

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft)  Down _ _) gs = gs { moveLeftHeld = True }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) gs = gs { moveRightHeld = True }
handleInput (EventKey (SpecialKey KeyLeft)  Up   _ _) gs = gs { moveLeftHeld = False }
handleInput (EventKey (SpecialKey KeyRight) Up   _ _) gs = gs { moveRightHeld = False }
handleInput (EventKey (SpecialKey KeyUp)    Down _ _) gs =
  gs { pendingJump = True, jumpHeld = True }
handleInput (EventKey (SpecialKey KeyUp)    Up   _ _) gs =
  gs { jumpHeld = False }
handleInput (EventKey (SpecialKey KeyCtrlR) Down _ _) gs = gs { sprintHeld = True }
handleInput (EventKey (SpecialKey KeyCtrlR) Up   _ _) gs = gs { sprintHeld = False }
handleInput _ gs = gs 

update :: Float -> GameState -> IO GameState
update dt gs = return $ updatePure dt gs

updatePure :: Float -> GameState -> GameState
updatePure dt gs =
  let updatedPlayer   = updatePlayer dt gs
      updatedEntities = map (updateEntity dt gs) (entities gs)
  in gs { player = updatedPlayer
        , entities = updatedEntities
        , pendingJump = False
        }

updateEntity :: Float -> GameState -> Entity -> Entity
updateEntity _  _  e@(EPlayer _) = e
updateEntity dt _  (EGoomba g)   = EGoomba (updatePhysics dt (updateMovable dt g))
updateEntity _  _  (EKoopa k)    = EKoopa k
updateEntity _  _  e             = e

updatePlayer :: Float -> GameState -> Player
updatePlayer dt gs =
  let p = player gs
      blockers = colliders (world gs) ++ mapMaybe entityCollider (entities gs)
      (jumpAccel, jumpTimer) = computeJumpHold dt gs p
      movedPlayer = applyMovement dt blockers gs jumpAccel p
      playerAfterHold =
        let jumpTime'
              | onGround movedPlayer = jumpAccelerationTime
              | otherwise            = jumpTimer
        in movedPlayer { playerJumpTime = jumpTime' }
  in if pendingJump gs && onGround playerAfterHold
        then playerAfterHold
               { playerVel = addVec (playerVel playerAfterHold) (0, jumpImpulse)
               , onGround  = False
               , playerJumpTime = 0
               }
        else playerAfterHold

computeJumpHold :: Float -> GameState -> Player -> (Vector, Float)
computeJumpHold dt gs Player { playerJumpTime = t0, onGround = grounded }
  | not (jumpHeld gs) = ((0, 0), jumpAccelerationTime)
  | grounded          = ((0, 0), jumpAccelerationTime)
  | t0 >= jumpAccelerationTime = ((0, 0), jumpAccelerationTime)
  | otherwise =
      let startTime = max 0 (min jumpAccelerationTime t0)
          endTime = min jumpAccelerationTime (startTime + dt)
          accelStart = jumpHoldValue startTime
          accelEnd   = jumpHoldValue endTime
          accelAvg   = 0.5 * (accelStart + accelEnd)
      in ((0, accelAvg), endTime)

jumpHoldValue :: Float -> Float
jumpHoldValue t
  | jumpAccelerationTime <= 0 = 0
  | otherwise =
      let factor = max 0 (1 - t / jumpAccelerationTime)
      in jumpStartAcceleration * factor

applyMovement :: Float -> [Collider] -> GameState -> Vector -> Player -> Player
applyMovement dt blockers gs jumpAccel p@Player { playerPos = pos, playerVel = vel, playerColliderSpec } =
  let gravityAccel = (0, gravityAcceleration)
      moveDirRaw = boolToDir (moveLeftHeld gs) (moveRightHeld gs)
      moveDir = clampInput moveDirRaw
      controlAccel =
        if moveDir == 0
          then (0, 0)
          else
            let controlMag
                  | onGround p = if sprintHeld gs then groundSprintAccel else groundWalkAccel
                  | otherwise =
                      let vx = fst vel
                          sameDir = moveDir * vx >= 0
                      in if sameDir then airMoveAccel else airMoveDecel
            in (moveDir * controlMag, 0)
      frictionCoeff = if onGround p then groundFrictionCoeff else airFrictionCoeff
      frictionAccel = (-frictionCoeff * fst vel, 0)
      totalAccel = gravityAccel
                   `addVec` controlAccel
                   `addVec` frictionAccel
                   `addVec` jumpAccel
      velAfterAccel = addVec vel (scaleVec totalAccel dt)
      displacement = scaleVec velAfterAccel dt
      (resolvedPos, flags) =
        case playerColliderSpec of
          Nothing   -> (addVecToPoint pos displacement, noCollisionFlags)
          Just spec -> resolveMovement spec pos displacement blockers
      velAfterCollision = applyCollisionFlags flags velAfterAccel
      onGround' = groundContact flags
  in p { playerPos = resolvedPos, playerVel = velAfterCollision, onGround = onGround' }

applyCollisionFlags :: CollisionFlags -> Vector -> Vector
applyCollisionFlags CollisionFlags { hitX, hitY } (vx, vy) =
  let vx' = if hitX then 0 else vx
      vy' = if hitY then 0 else vy
  in (vx', vy')

data SweepResult
  = SweepClear Point
  | SweepHit
      { safePos    :: Point
      , hitPos     :: Point
      , hitBlocker :: Collider
      }

data Axis = AxisX | AxisY deriving (Eq, Show)

data CollisionFlags = CollisionFlags
  { hitX          :: Bool
  , hitY          :: Bool
  , groundContact :: Bool
  } deriving (Eq, Show)

noCollisionFlags :: CollisionFlags
noCollisionFlags = CollisionFlags False False False

combineFlags :: CollisionFlags -> CollisionFlags -> CollisionFlags
combineFlags a b = CollisionFlags
  { hitX          = hitX a || hitX b
  , hitY          = hitY a || hitY b
  , groundContact = groundContact a || groundContact b
  }

resolveMovement :: ColliderSpec -> Point -> Vector -> [Collider]
                -> (Point, CollisionFlags)
resolveMovement spec start displacement blockers =
  go 3 start displacement noCollisionFlags
  where
    go :: Int -> Point -> Vector -> CollisionFlags -> (Point, CollisionFlags)
    go 0 pos _ flags = (pos, flags)
    go _ pos disp flags | nearZeroVec disp = (pos, flags)
    go attempts pos disp flags =
      let endPos = addVecToPoint pos disp
          sweep = sweepSegment spec pos disp blockers 5
      in case sweep of
           SweepClear finalPos -> (finalPos, flags)
           SweepHit { safePos = safe, hitPos = hit, hitBlocker = blocker } ->
             let entityAABB = specToCollider hit spec
                 (normal, axis) = collisionNormal entityAABB blocker
                 remainder = subPoints endPos safe
                 remainingAfterCollision = removeNormalComponent remainder axis
                 flagsThisCollision = collisionFlagsFor axis normal
                 combinedFlags = combineFlags flags flagsThisCollision
             in if nearZeroVec remainingAfterCollision
                  then (safe, combinedFlags)
                  else go (attempts - 1) safe remainingAfterCollision combinedFlags

removeNormalComponent :: Vector -> Axis -> Vector
removeNormalComponent (_, dy) AxisX = (0, dy)
removeNormalComponent (dx, _) AxisY = (dx, 0)

collisionFlagsFor :: Axis -> Vector -> CollisionFlags
collisionFlagsFor axis normal =
  let ground = isGroundNormal normal
  in case axis of
       AxisX -> CollisionFlags True False False
       AxisY -> CollisionFlags False True ground

isGroundNormal :: Vector -> Bool
isGroundNormal (nx, ny) =
  ny > 0 && ny >= abs nx

collisionNormal :: Collider -> Collider -> (Vector, Axis)
collisionNormal (AABB (ex, ey) ew eh) (AABB (bx, by) bw bh) =
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

nearZeroVec :: Vector -> Bool
nearZeroVec (x, y) = abs x < 1e-4 && abs y < 1e-4

sweepSegment :: ColliderSpec -> Point -> Vector -> [Collider] -> Int -> SweepResult
sweepSegment spec start disp blockers steps = go 1 start
  where
    go i prevPos
      | i > steps = SweepClear (addVecToPoint start disp)
      | otherwise =
          let t = fromIntegral i / fromIntegral steps
              current = addVecToPoint start (scaleVec disp t)
              collider = specToCollider current spec
              hit = find (Collision.collides collider) blockers
          in case hit of
               Just blocker -> SweepHit { safePos = prevPos, hitPos = current, hitBlocker = blocker }
               Nothing      -> go (i + 1) current

boolToDir :: Bool -> Bool -> Float
boolToDir left right = (if right then 1.0 else 0.0) - (if left then 1.0 else 0.0)

clampInput :: Float -> Float
clampInput x
  | x > 1     = 1
  | x < -1    = -1
  | otherwise = x

addVec :: Vector -> Vector -> Vector
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scaleVec :: Vector -> Float -> Vector
scaleVec (x, y) s = (x * s, y * s)

addVecToPoint :: Point -> Vector -> Point
addVecToPoint (x, y) (dx, dy) = (x + dx, y + dy)

subPoints :: Point -> Point -> Vector
subPoints (x1, y1) (x0, y0) = (x1 - x0, y1 - y0)

class Movable e where
  getPos :: e -> Point
  setPos :: e -> Point -> e
  getVel :: e -> Vector 
  setVel :: e -> Vector -> e

class Movable e => Rigidbody e where
  getLevelCollisions :: e -> TileMap -> [TileMap]

updateMovable :: Movable e => Float -> e -> e
updateMovable dt e =
  let (vx, vy)   = getVel e
      (x,  y)    = getPos e
   in setPos e (x + vx * dt, y + vy * dt)

updatePhysics :: Rigidbody e => Float -> e -> e
updatePhysics dt e =
  let (vx, vy) = getVel e
  in setVel e (vx, vy + gravityAcceleration * dt)


instance Movable Player where
  getPos       = playerPos
  setPos p pos = p { playerPos = pos }
  getVel       = playerVel
  setVel p vel = p { playerVel = vel }

instance Rigidbody Player where
  getLevelCollisions = undefined

instance Movable Goomba where
  getPos       = goombaPos 
  setPos g pos = g { goombaPos = pos }
  getVel       = goombaVel
  setVel g vel = g { goombaVel = vel }

instance Rigidbody Goomba where
  getLevelCollisions = undefined
