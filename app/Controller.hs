module Controller where

import Model
import qualified Model as M
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (mapMaybe, maybeToList, isJust)
import qualified Collision
import Data.List (find, nub)

gravityAcceleration :: Float
gravityAcceleration = -35

jumpImpulse :: Float
jumpImpulse = 8

jumpHoldAccelStart :: Float
jumpHoldAccelStart = 40

jumpHoldDuration :: Float
jumpHoldDuration = 0.6

groundWalkAccel, groundSprintAccel, groundMoveDecel, airMoveAccel, airMoveDecel :: Float
groundWalkAccel = 65
groundSprintAccel = 110
groundMoveDecel = 125
airMoveAccel = 22
airMoveDecel = 28

goombaWalkSpeed :: Float
goombaWalkSpeed = 3

goombaMoveAccel :: Float
goombaMoveAccel = 20

-- Zoom bounds for tileSize (pixels per tile)
minTileSize, maxTileSize :: Int
minTileSize = 6
maxTileSize = 72

groundFrictionCoeff, airFrictionCoeff :: Float
groundFrictionCoeff = 12
airFrictionCoeff = 1.5

upVector :: Vector
upVector = (0, 1)

input :: Event -> GameState -> IO GameState
input e gs = return $ handleInput e gs

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft)  Down _ _) gs = gs { moveLeftHeld = True }
handleInput (EventKey (SpecialKey KeyRight) Down _ _) gs = gs { moveRightHeld = True }
handleInput (EventKey (SpecialKey KeyLeft)  Up   _ _) gs = gs { moveLeftHeld = False }
handleInput (EventKey (SpecialKey KeyRight) Up   _ _) gs = gs { moveRightHeld = False }
-- Zoom controls
handleInput (EventKey (Char '+') Down _ _) gs = adjustTileSize 1 gs
handleInput (EventKey (Char '=') Down _ _) gs = adjustTileSize 1 gs
handleInput (EventKey (Char '-') Down _ _) gs = adjustTileSize (-1) gs
handleInput (EventKey (Char '_') Down _ _) gs = adjustTileSize (-1) gs
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
updateEntity dt gs (EGoomba g)   = EGoomba (updateGoomba dt gs g)
updateEntity _  _  (EKoopa k)    = EKoopa k
updateEntity _  _  e             = e

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
          in playerAfterHold
               { playerVel      = addVec (playerVel playerAfterHold) impulse
               , onGround       = False
               , playerJumpTime = 0
               , playerJumpDir  = launchDir
               , playerSlide    = Nothing
               }
        else playerAfterHold

computeJumpHold :: Float -> GameState -> Player -> (Vector, Float)
computeJumpHold dt gs Player { playerJumpTime = t0, onGround = grounded, playerJumpDir = initDir }
  | not (jumpHeld gs)        = ((0, 0), jumpHoldDuration)
  | grounded                 = ((0, 0), jumpHoldDuration)
  | t0 >= jumpHoldDuration   = ((0, 0), jumpHoldDuration)
  | otherwise =
      let startTime   = clampInterval t0
          endTime     = min jumpHoldDuration (startTime + dt)
          accelStart  = jumpHoldValue startTime
          accelEnd    = jumpHoldValue endTime
          dirStart    = jumpDirectionAt initDir startTime
          dirEnd      = jumpDirectionAt initDir endTime
          accelVecStart = scaleVec dirStart accelStart
          accelVecEnd   = scaleVec dirEnd   accelEnd
          avgAccelVec   = scaleVec (addVec accelVecStart accelVecEnd) 0.5
      in (avgAccelVec, endTime)
  where
    clampInterval t = max 0 (min jumpHoldDuration t)

jumpHoldValue :: Float -> Float
jumpHoldValue t
  | jumpHoldDuration <= 0 = 0
  | otherwise =
      let factor = max 0 (1 - t / jumpHoldDuration)
      in jumpHoldAccelStart * factor

jumpDirectionAt :: Vector -> Float -> Vector
jumpDirectionAt initDir t
  | jumpHoldDuration <= 0 = upVector
  | otherwise =
      let alpha = clamp01 (t / jumpHoldDuration)
          blended = addVec (scaleVec initDir (1 - alpha)) (scaleVec upVector alpha)
      in normalizeVec blended

applyMovement :: Float -> [Collider] -> GameState -> Vector -> Player -> Player
applyMovement dt blockers gs jumpAccel p@Player { playerPos = pos
                                                , playerVel = vel
                                                , playerColliderSpec
                                                , playerSlide = prevSlide
                                                } =
  let gravityAccel = (0, gravityAcceleration)
      moveDirRaw = boolToDir (moveLeftHeld gs) (moveRightHeld gs)
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
                      let vx = fst vel
                          sameDir = moveDir * vx >= 0
                      in if sameDir then airMoveAccel else airMoveDecel
            in (moveDir * controlMag, 0)
      airDrag = (- (airFrictionCoeff * fst vel), 0)
      totalAccel = gravityAccel
                   `addVec` controlAccel
                   `addVec` airDrag
                   `addVec` jumpAccel
      velAfterAccel = addVec vel (scaleVec totalAccel dt)
      displacement = scaleVec velAfterAccel dt
      (resolvedPos, flags) =
        case playerColliderSpec of
          Nothing   -> (addVecToPoint pos displacement, noCollisionFlags)
          Just spec -> resolveMovement spec pos displacement blockers
      velAfterCollision = applyCollisionFlags flags velAfterAccel
      contacts = contactNormals flags
      newSlide = determineSlide playerColliderSpec resolvedPos blockers contacts prevSlide
      frictionContacts = contacts ++ maybeToList newSlide
      vyAfterCollision = snd velAfterCollision
      contactDrag
        | vyAfterCollision > 0 = (0, 0)
        | otherwise            = contactFrictionAccel frictionContacts velAfterCollision
      velFinal = addVec velAfterCollision (scaleVec contactDrag dt)
      onGround' = groundContact flags
  in p { playerPos = resolvedPos
       , playerVel = velFinal
       , onGround = onGround'
       , playerSlide = newSlide
       }

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
  { hitX           :: Bool
  , hitY           :: Bool
  , groundContact  :: Bool
  , contactNormals :: [Vector]
  } deriving (Eq, Show)

noCollisionFlags :: CollisionFlags
noCollisionFlags = CollisionFlags False False False []

combineFlags :: CollisionFlags -> CollisionFlags -> CollisionFlags
combineFlags a b = CollisionFlags
  { hitX           = hitX a || hitX b
  , hitY           = hitY a || hitY b
  , groundContact  = groundContact a || groundContact b
  , contactNormals = contactNormals a ++ contactNormals b
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
       AxisX -> CollisionFlags True False ground [normal]
       AxisY -> CollisionFlags False True ground [normal]

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

contactFrictionAccel :: [Vector] -> Vector -> Vector
contactFrictionAccel normals vel =
  foldl addVec (0, 0)
    [ scaleVec tangent (- (groundFrictionCoeff * dotVec vel tangent))
    | tangent <- map tangentFromNormal (unique normals)
    ]
  where
    unique = nub

tangentFromNormal :: Vector -> Vector
tangentFromNormal (nx, ny) = normalizeVec (-ny, nx)

dotVec :: Vector -> Vector -> Float
dotVec (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

normalizeVec :: Vector -> Vector
normalizeVec (x, y) =
  let mag = sqrt (x * x + y * y)
  in if mag < 1e-6 then (0, 0) else (x / mag, y / mag)

determineSlide :: Maybe ColliderSpec -> Point -> [Collider] -> [Vector] -> Maybe Vector -> Maybe Vector
determineSlide spec pos blockers contacts prevSlide =
  case find isWallNormal contacts of
    Just wall -> Just wall
    Nothing   ->
      case (spec, prevSlide) of
        (Just s, Just normal)
          | isWallNormal normal
          , stillTouching s pos blockers normal -> Just normal
        _ -> Nothing

computeJumpLaunchDir :: Player -> Vector
computeJumpLaunchDir Player { onGround, playerSlide, playerJumpDir } =
  normalizeVec $ case () of
    _ | onGround -> upVector
      | Just normal <- playerSlide -> addVec normal upVector
      | otherwise -> playerJumpDir

isWallNormal :: Vector -> Bool
isWallNormal (nx, ny) = abs nx > 0.5 && abs ny < 0.75

stillTouching :: ColliderSpec -> Point -> [Collider] -> Vector -> Bool
stillTouching spec pos blockers normal =
  let probePos = addVecToPoint pos (scaleVec (negateVec normal) slideProbeDistance)
      testCollider = specToCollider probePos spec
  in any (Collision.collides testCollider) blockers

slideProbeDistance :: Float
slideProbeDistance = 0.02

clamp01 :: Float -> Float
clamp01 x
  | x < 0     = 0
  | x > 1     = 1
  | otherwise = x

boolToDir :: Bool -> Bool -> Float
boolToDir left right = (if right then 1.0 else 0.0) - (if left then 1.0 else 0.0)

clampInput :: Float -> Float
clampInput x
  | x > 1     = 1
  | x < -1    = -1
  | otherwise = x

clampGoombaVelocity :: Vector -> Vector
clampGoombaVelocity (vx, vy) =
  let vx' = max (-goombaWalkSpeed) (min goombaWalkSpeed vx)
  in (vx', vy)

addVec :: Vector -> Vector -> Vector
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scaleVec :: Vector -> Float -> Vector
scaleVec (x, y) s = (x * s, y * s)

addVecToPoint :: Point -> Vector -> Point
addVecToPoint (x, y) (dx, dy) = (x + dx, y + dy)

subPoints :: Point -> Point -> Vector
subPoints (x1, y1) (x0, y0) = (x1 - x0, y1 - y0)

negateVec :: Vector -> Vector
negateVec (x, y) = (-x, -y)

-- Adjust zoom by changing tileSize, clamped to [minTileSize, maxTileSize]
adjustTileSize :: Int -> GameState -> GameState
adjustTileSize delta gs =
  let cur = tileSize gs
      newVal = clampTileSize (cur + delta)
  in gs { tileSize = newVal }

clampTileSize :: Int -> Int
clampTileSize n = max minTileSize (min maxTileSize n)

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
updateGoomba :: Float -> GameState -> Goomba -> Goomba
updateGoomba dt gs g@Goomba { goombaPos = pos
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
          (resolvedPos, flags) = resolveMovement spec pos displacement blockers
          velAfterCollision    = applyCollisionFlags flags velAfterAccel
          contactDrag          = contactFrictionAccel (contactNormals flags) velAfterCollision
          velWithFriction      = addVec velAfterCollision (scaleVec contactDrag dt)
          velLimited           = clampGoombaVelocity velWithFriction
          hitWall              = hitX flags
          newDir               = if hitWall then flipDir dir else dir
          adjVx                = if hitWall then 0 else fst velLimited
          velFinal             = (adjVx, snd velLimited)
      in g { goombaPos     = resolvedPos
           , goombaVel     = velFinal
           , goombaDir     = newDir
           , goombaOnGround = groundContact flags
           }
  where
    gravityAccel  = (0, gravityAcceleration)
    accelSign     = case dir of { M.Left -> -1; M.Right -> 1 }
    goombaAccel   = (fromIntegral accelSign * goombaMoveAccel, 0)
    airDrag       = (-airFrictionCoeff * fst vel, 0)
    totalAccel    = gravityAccel `addVec` goombaAccel `addVec` airDrag

flipDir :: M.MoveDir -> M.MoveDir
flipDir M.Left  = M.Right
flipDir M.Right = M.Left
