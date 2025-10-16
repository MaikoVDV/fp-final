module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (mapMaybe)
import qualified Collision

gravityAcceleration :: Float
gravityAcceleration = -9.81

playerMoveSpeed :: Float
playerMoveSpeed = 5 -- tiles per second

input :: Event -> GameState -> IO GameState
input e gs = return $ handleInput e gs

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyLeft)  Down _ _) gs = movePlayer (-1) gs
handleInput (EventKey (SpecialKey KeyRight) Down _ _) gs = movePlayer 1 gs
handleInput (EventKey (SpecialKey KeyLeft)  Up   _ _) gs = stopPlayer gs
handleInput (EventKey (SpecialKey KeyRight) Up   _ _) gs = stopPlayer gs
handleInput _ gs = gs 

update :: Float -> GameState -> IO GameState
update dt gs = return $ updatePure dt gs

updatePure :: Float -> GameState -> GameState
updatePure dt gs =
  let updatedPlayer   = updatePlayer dt gs
      updatedEntities = map (updateEntity dt gs) (entities gs)
  in gs { player = updatedPlayer, entities = updatedEntities }

updateEntity :: Float -> GameState -> Entity -> Entity
updateEntity _  _  e@(EPlayer _) = e
updateEntity dt _  (EGoomba g)   = EGoomba (updatePhysics dt (updateMovable dt g))
updateEntity _  _  (EKoopa k)    = EKoopa k
updateEntity _  _  e             = e

updatePlayer :: Float -> GameState -> Player
updatePlayer dt gs =
  let p = player gs
      blockers = colliders (world gs) ++ mapMaybe entityCollider (entities gs)
  in applyMovement dt blockers p

applyMovement :: Float -> [Collider] -> Player -> Player
applyMovement dt blockers p@Player { playerPos = pos, playerVel = vel, playerColliderSpec } =
  let gravityAccel = (0, gravityAcceleration)
      velAfterAccel = addVec vel (scaleVec gravityAccel dt)
      proposedDisplacement = scaleVec velAfterAccel dt
      proposedPos = addVecToPoint pos proposedDisplacement
      (resolvedPos, collided, collidedWithGround) =
        case playerColliderSpec of
          Nothing   -> (proposedPos, False, False)
          Just spec -> resolveAlongPath spec pos proposedPos blockers
      velAfterCollision =
        if collided
          then (fst velAfterAccel, if collidedWithGround then 0 else snd velAfterAccel)
          else velAfterAccel
      onGround' = collidedWithGround
  in p { playerPos = resolvedPos, playerVel = velAfterCollision, onGround = onGround' }

resolveAlongPath :: ColliderSpec -> Point -> Point -> [Collider]
                 -> (Point, Bool, Bool)
resolveAlongPath spec start end blockers
  | start == end = (start, False, False)
  | otherwise    =
      let points = samplePointsAlongLine 5 start end
          (lastSafe, collided) = walk start points
          finalPos = if collided then lastSafe else end
          groundCollision = collided && snd end < snd start
      in (finalPos, collided, groundCollision)
  where
    walk lastSafe [] = (lastSafe, False)
    walk lastSafe (pt:pts) =
      let collider = specToCollider pt spec
          collision = any (Collision.collides collider) blockers
      in if collision
           then (lastSafe, True)
           else walk pt pts

samplePointsAlongLine :: Int -> Point -> Point -> [Point]
samplePointsAlongLine steps (x0, y0) (x1, y1) =
  [ (x0 + (x1 - x0) * t, y0 + (y1 - y0) * t)
  | let stepF = fromIntegral steps
  , i <- [steps, steps - 1 .. 1]
  , let t = fromIntegral i / stepF
  ]

addVec :: Vector -> Vector -> Vector
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

scaleVec :: Vector -> Float -> Vector
scaleVec (x, y) s = (x * s, y * s)

addVecToPoint :: Point -> Vector -> Point
addVecToPoint (x, y) (dx, dy) = (x + dx, y + dy)

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


movePlayer :: Float -> GameState -> GameState
movePlayer dir gs =
  let p      = player gs
      (_, vY) = getVel p
      p'     = setVel p (dir * playerMoveSpeed, vY)
  in gs { player = p' }

stopPlayer :: GameState -> GameState
stopPlayer gs =
  let p      = player gs
      (_, vY) = getVel p
      p'     = setVel p (0, vY)
  in gs { player = p' }
