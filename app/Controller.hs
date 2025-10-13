module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game

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
updatePure dt gs = gs { entities = map (updateEntity dt) (entities gs)}

updateEntity :: Float -> Entity -> Entity
updateEntity dt (EPlayer p) = EPlayer (updatePhysics dt (updateMovable dt p))
updateEntity dt (EGoomba g) = EGoomba (updatePhysics dt (updateMovable dt g))
updateEntity _  e           = e

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
  in setVel e (vx, vy - 18 * 9.81 * dt)


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
movePlayer dir gs = gs { entities = map go (entities gs) }
  where
    go (EPlayer p) = 
      let (_, vY) = getVel p
      in EPlayer (setVel p (dir * 100, vY))
    go e           = e

stopPlayer :: GameState -> GameState
stopPlayer gs = gs { entities = map go (entities gs) }
  where
    go (EPlayer p) = 
      let (_, vY) = getVel p
      in  EPlayer (setVel p (0, vY))
    go e           = e