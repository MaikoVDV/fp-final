module Controller.Update where

import Graphics.Gloss
import Data.Maybe (mapMaybe, isJust)

import Model.Types
import Model.TypesState
import Model.Collider
import Model.Config
import Model.InfiniteWorld (ensureInfiniteSegments)
import Model.InitialState (baseTilePixelSizeForScreen)
import Model.Entity (setPlayerHealth)

import Controller.Input
import Controller.Collision
import Controller.Movement

import MathUtils
import Model.WorldMap (polylineLength, Edge(..), WorldMap(..), MapNode(..), EdgeDir(..), NodeState(..), NodeType(..), NodeId(..))
import Model.WorldMapCodec (saveWorldMapFile)

import Model.Scores (saveLives)

update :: Float -> AppState -> IO AppState
update _  menuState@(Menu _) = return menuState
update dt (Playing gs)
  | paused gs =
      case nextState gs of
        NPlaying     -> return (Playing gs)
        NFinishLevel -> case currentMapState gs of
                          Just ms -> do
                            let ms' = unlockAfterFinish ms
                            saveWorldMapFile (wmFilePath ms') (wmWorldMap ms')
                            return (WorldMapScreen ms')
                          Nothing -> return (Menu $ menuState gs)
        _            -> return (Menu $ menuState gs)
  | otherwise = do
      gsPrepared <- ensureInfiniteSegments gs
      let gs' = updateGame dt gsPrepared
      gsPost <- ensureInfiniteSegments gs'
      -- Persist lives if changed
      let l0 = playerLives gsPrepared
          l1 = playerLives gsPost
      if l1 /= l0 then saveLives l1 else return ()
      -- If died with no lives left: restore 5 lives and reset world progress unless boss defeated
      gsFinal <- case nextState gsPost of
                  NDeath | playerLives gsPost <= 0 -> do
                    let newLives = 5
                    saveLives newLives
                    case currentMapState gsPost of
                      Just ms -> do
                        let wm = wmWorldMap ms
                            bossCompleted = any (\n -> nodeType n == Boss && nodeState n == Completed) (nodes wm)
                        if not bossCompleted
                          then do
                            -- Reset nodes to Locked, keep start node (id 0) Unlocked; reset edges; unlock edges from start
                            let startId = NodeId 0
                                resetNode n = if nodeId n == startId then n { nodeState = Unlocked }
                                              else n { nodeState = Locked }
                                allowsFrom e nid = case dir e of
                                  Undirected -> a e == nid || b e == nid
                                  Both       -> a e == nid || b e == nid
                                  AtoB       -> a e == nid
                                  BtoA       -> b e == nid
                                edges0 = [ e { unlocked = False } | e <- edges wm ]
                                edges1 = [ if allowsFrom e startId then e { unlocked = True } else e | e <- edges0 ]
                                wm' = wm { nodes = map resetNode (nodes wm), edges = edges1 }
                            saveWorldMapFile (wmFilePath ms) wm'
                          else return ()
                        return gsPost { playerLives = newLives }
                      Nothing -> return gsPost { playerLives = newLives }
                  _ -> return gsPost
      case nextState gsFinal of
          NPlaying     -> return . Playing $ gsFinal
          NFinishLevel -> case currentMapState gsFinal of
                            Just ms -> do
                              let ms' = unlockAfterFinish ms
                              saveWorldMapFile (wmFilePath ms') (wmWorldMap ms')
                              return (WorldMapScreen ms')
                            Nothing -> return (Menu $ menuState gsFinal)
          NDeath       -> case currentMapState gsFinal of
                            Just ms -> return (WorldMapScreen ms)
                            Nothing -> return (Menu $ menuState gsFinal)
          _            -> return (Menu $ menuState gsFinal)
update _ (Building bs)      = return (Building bs)
update dt (WorldMapScreen ms) = return (WorldMapScreen (updateMap dt ms))

updateMap :: Float -> MapState -> MapState
updateMap _ ms@MapState { wmAlong = Nothing } = ms
updateMap dt ms@MapState { wmAlong = Just (eid, pts, dest, t), wmSpeed } =
  let len = polylineLength pts
  in if len <= 1e-3
        then ms { wmCursor = dest, wmAlong = Nothing }
        else
          let step = wmSpeed * dt / len
              t' = t + step
          in if t' >= 1
                then ms { wmCursor = dest, wmAlong = Nothing }
                else ms { wmAlong = Just (eid, pts, dest, t') }

-- Unlock neighbors and mark current node completed
unlockAfterFinish :: MapState -> MapState
unlockAfterFinish ms@MapState { wmWorldMap = wm, wmCursor = cur } =
  let edges1 = [ if allowsFrom e cur then e { unlocked = True } else e | e <- edges wm ]
      neighborIds = [ other e cur | e <- edges wm, allowsFrom e cur ]
      -- Hubs among immediate neighbors
      isHubId nid = case filter ((== nid) . nodeId) (nodes wm) of
                      (n:_) -> case nodeType n of { Hub -> True; _ -> False }
                      _     -> False
      hubIds = [ nid | nid <- neighborIds, isHubId nid ]
      -- Unlock all edges emanating from unlocked hubs as well
      unlockFromHub e = any (allowsFrom e) hubIds
      edges2 = [ if unlockFromHub e then e { unlocked = True } else e | e <- edges1 ]
      -- Also unlock nodes reachable from those hubs
      neighborIdsFromHubs = [ other e h | e <- edges wm, h <- hubIds, allowsFrom e h ]
      nodes' = [ updateNode n | n <- nodes wm ]
      updateNode n
        | nodeId n == cur = n { nodeState = Completed }
        | nodeId n `elem` (neighborIds ++ neighborIdsFromHubs) = case nodeState n of
            Locked -> n { nodeState = Unlocked }
            _      -> n
        | otherwise = n
      wm' = wm { nodes = nodes', edges = edges2 }
  in ms { wmWorldMap = wm' }
  where
    allowsFrom e nid = case dir e of
      Undirected -> a e == nid || b e == nid
      Both       -> a e == nid || b e == nid
      AtoB       -> a e == nid
      BtoA       -> b e == nid
    other e nid = if a e == nid then b e else a e

-- Main game update function
updateGame :: Float -> GameState -> GameState
updateGame dt =
  handleCollisionEvents
  . resolveInterEnemyOverlaps
  . updateEntities dt
  . clearJump
  . updatePlayer dt
  . checkVoidDeath
  . incrementFrame
  where
    incrementFrame :: GameState -> GameState
    incrementFrame gs@GameState { frameCount }
      = gs {frameCount = frameCount + 1, nextState = NPlaying}

    clearJump ::GameState -> GameState
    clearJump gs = gs { pendingJump = False }

    -- Kill the player if they've fallen out of the world
    checkVoidDeath :: GameState -> GameState
    checkVoidDeath gs@GameState { world = World { grid }, player = Player { playerPos = (_, py) } }
      | null grid = gs
      | py < fromIntegral (negate (length grid)) = setPlayerHealth gs 0
      | otherwise = gs


updatePlayer :: Float -> GameState -> GameState
updatePlayer dt gs =
  let p = player gs
      -- Phase-through enemies: only world blocks the player
      blockers = colliders (world gs) ++ blockingEntityColliders (entities gs)
      (jumpAccel, jumpTimer) = computeJumpHold dt gs p
      movedPlayer = applyMovement dt blockers gs jumpAccel p
      -- Advance animation clock proportionally to horizontal speed and dt,
      -- only while a left/right key is held.
      vxAbs = abs (fst (playerVel movedPlayer))
      movingInput = moveLeftHeld movedPlayer || moveRightHeld movedPlayer
      animRateScale = 2.5 -- frames per second at 1 unit/s
      animClock'
        | movingInput = playerAnimClock movedPlayer + vxAbs * animRateScale * dt
        | otherwise   = playerAnimClock movedPlayer
      sliding = isJust (playerSlide movedPlayer)
      jumpTime'
        | onGround movedPlayer || sliding = jumpHoldDuration
        | otherwise            = jumpTimer
      jumpDir'
        | onGround movedPlayer = upVector
        | otherwise            = playerJumpDir movedPlayer
      -- Reset jump count when grounded; otherwise keep current
      jumpsAvailable
        | onGround movedPlayer || sliding = maxJumps
        | otherwise            = jumpsLeft movedPlayer
      -- Countdown stomp jump boost timer
      stompBoostTimeLeft = max 0 (stompJumpTimeLeft movedPlayer - dt)
      inv' = max 0 (invulnTimeLeft movedPlayer - dt)
      playerAfterHold = movedPlayer
        { playerJumpTime = jumpTime'
        , playerJumpDir  = jumpDir'
        , stompJumpTimeLeft = stompBoostTimeLeft
        , playerAnimClock = animClock'
        , invulnTimeLeft = inv'
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
              , stompJumpTimeLeft = min stompBoostTimeLeft 0
              }
            }
        else gs { player = playerAfterHold { jumpsLeft = jumpsAvailable } }

  where
    blockingEntityColliders :: [Entity] -> [Collider]
    blockingEntityColliders es = mapMaybe entityCollider (filter isBlocking es)
      where
        isBlocking :: Entity -> Bool
        isBlocking e = case e of
          -- Enemies do not block the player
          EGoomba  _ _ -> False
          EKoopa   _ _ -> False
          EPlatform _   -> True
          EPowerup _ _ -> False
          ECoin    _ _ -> False


updateEntities :: Float -> GameState -> GameState
updateEntities dt gs = gs { entities = map (updateIfActive dt gs) (entities gs) }
  where
    updateIfActive delta state entity
      | entityWithinActiveBounds state entity = updateEntity delta state entity
      | otherwise = entity

entityWithinActiveBounds :: GameState -> Entity -> Bool
entityWithinActiveBounds gs entity =
  case entityPosition entity of
    Nothing   -> True
    Just pos  -> pointInBounds (activationBounds gs) pos

activationBounds :: GameState -> (Float, Float, Float, Float)
activationBounds GameState { player = Player { playerPos = (px, py) }, screenSize = (screenWInt, screenHInt), tileZoom } =
  let tilePixels = baseTilePixelSizeForScreen (screenWInt, screenHInt) * tileZoom * scaleFactor
      screenWidth  = fromIntegral screenWInt
      screenHeight = fromIntegral screenHInt
      desiredPlayerScreenFraction = 1 / 3 :: Float
      targetPlayerY = (-0.5 + desiredPlayerScreenFraction) * screenHeight
      halfWidthTiles = screenWidth / (2 * tilePixels)
      offsetMin = (-screenHeight / 2 - targetPlayerY) / tilePixels
      offsetMax = ( screenHeight / 2 - targetPlayerY) / tilePixels
      margin = 2.5
      minX = px - halfWidthTiles - margin
      maxX = px + halfWidthTiles + margin
      minY = py + offsetMin - margin
      maxY = py + offsetMax + margin
  in (minX, maxX, minY, maxY)

pointInBounds :: (Float, Float, Float, Float) -> Point -> Bool
pointInBounds (minX, maxX, minY, maxY) (x, y) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

entityPosition :: Entity -> Maybe Point
entityPosition (EGoomba _ Goomba { goombaPos }) = Just goombaPos
entityPosition (EKoopa  _ Koopa  { koopaPos  }) = Just koopaPos
entityPosition (EPowerup _ Powerup { powerupPos }) = Just powerupPos
entityPosition (ECoin    _ Coin    { coinPos    }) = Just coinPos
entityPosition _ = Nothing

-- Handle updating different types of entities separately
updateEntity :: Float -> GameState -> Entity -> Entity
updateEntity dt gs (EGoomba  gId  g)  = EGoomba  gId  (updateGoomba  dt gs gId  g)
updateEntity dt gs (EKoopa   kId  k)  = EKoopa   kId  (updateKoopa   dt gs kId  k)
updateEntity dt gs (EPowerup puId pu) = EPowerup puId (updatePowerup dt gs puId pu)
updateEntity _  _  (ECoin    cId  c)  = ECoin    cId  c
updateEntity _  _  e                   = e

updateGoomba :: Float -> GameState -> Int -> Goomba -> Goomba
updateGoomba dt gs gId g@Goomba
  { goombaPos = pos
  , goombaVel = vel
  , goombaColSpec = colSpec
  , goombaDir = dir
  , goombaMode = mode
  } =
  case colSpec of
    Nothing ->
      let velAfterAccel = addVec vel (scaleVec totalAccel dt)
          velLimited    = clampGoombaVelocity velAfterAccel
          newPos        = addVecToPoint pos (scaleVec velLimited dt)
          (mode', velFinal) = advanceMode mode velLimited
      in g { goombaPos = newPos
           , goombaVel = velFinal
           , goombaMode = mode'
           }
    Just spec ->
      let velAfterAccel   = addVec vel (scaleVec totalAccel dt)
          displacement    = scaleVec velAfterAccel dt
          -- Only block against world and player; enemy separation handled globally
          -- Phase-through player: only world blocks the goomba
          blockers        = colliders (world gs)
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
          newDir               = if isWalking then newDirBase else dir
          adjVxWalk            = if hitWall then 0 else fst velLimited
          vxMode               = if isWalking then adjVxWalk else 0
          velFinal0            = (vxMode, snd velLimited)
          (mode', velFinal)    = advanceMode mode velFinal0
      in g { goombaPos        = resolvedPos
           , goombaVel        = velFinal
           , goombaDir        = newDir
           , goombaOnGround   = groundContact flags
           , goombaCollisions = events
           , goombaMode       = mode'
           }
  where
    gravityAccel  = (0, gravityAcceleration)
    dirSign       = case dir of { DirLeft -> -1.0; DirRight -> 1.0 }
    isWalking     = case mode of { GWalking -> True; _ -> False }
    goombaAccel   = if isWalking then (dirSign * goombaMoveAccel, 0) else (0, 0)
    airDrag       = (- (airFrictionCoeff * fst vel), 0)
    totalAccel    = gravityAccel `addVec` goombaAccel `addVec` airDrag
    clampGoombaVelocity (vx, vy) =
      let vx' = max (-goombaWalkSpeed) (min goombaWalkSpeed vx)
      in (vx', vy)
    flipDir DirLeft  = DirRight
    flipDir DirRight = DirLeft
    -- Advance shelled timer and ensure x-velocity is 0 while shelled
    advanceMode m (vx, vy) = case m of
      GWalking      -> (GWalking, (vx, vy))
      GShelled ttl  -> let ttl' = max 0 (ttl - dt)
                       in if ttl' <= 0
                            then (GWalking, (0, vy))
                            else (GShelled ttl', (0, vy))

updateKoopa :: Float -> GameState -> Int -> Koopa -> Koopa
updateKoopa dt gs gId k@Koopa
  { koopaPos = pos
  , koopaVel= vel
  , koopaColSpec = colSpec
  , koopaDir = dir
  } =
  case colSpec of
    Nothing ->
      let velAfterAccel = addVec vel (scaleVec totalAccel dt)
          velLimited    = clampGoombaVelocity velAfterAccel
          newPos        = addVecToPoint pos (scaleVec velLimited dt)
      in k { koopaPos = newPos
           , koopaVel = velLimited
           }
    Just spec ->
      let velAfterAccel   = addVec vel (scaleVec totalAccel dt)
          displacement    = scaleVec velAfterAccel dt
          -- Only block against world and player; enemy separation handled globally
          -- Phase-through player: only world blocks the goomba
          blockers        = colliders (world gs)
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
          newDir            = if hitWall then flipDir dir else dir
          vxWalk            = if hitWall then 0 else fst velLimited
          velFinal            = (vxWalk, snd velLimited)
      in k { koopaPos        = resolvedPos
           , koopaVel        = velFinal
           , koopaDir        = newDir
           , koopaOnGround   = groundContact flags
           , koopaCollisions = events
           }
  where
    gravityAccel  = (0, gravityAcceleration)
    dirSign       = case dir of { DirLeft -> -1.0; DirRight -> 1.0 }
    goombaAccel   = (dirSign * goombaMoveAccel, 0) 
    airDrag       = (- (airFrictionCoeff * fst vel), 0)
    totalAccel    = gravityAccel `addVec` goombaAccel `addVec` airDrag
    clampGoombaVelocity (vx, vy) =
      let vx' = max (-goombaWalkSpeed) (min goombaWalkSpeed vx)
      in (vx', vy)
    flipDir DirLeft  = DirRight
    flipDir DirRight = DirLeft
  

-- Entity-Entity separation handled in resolveInterEnemyOverlaps
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
          blockers        = colliders (world gs) -- doesn't collide with player or entities
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
          adjVx                = if hitWall then 0 else fst velLimited
          velFinal             = (adjVx, snd velLimited)
      in pu { powerupPos        = resolvedPos
            , powerupVel        = velFinal
            , powerupDir        = newDirBase
            , powerupCollisions = events
            }
  where
    gravityAccel  = (0, gravityAcceleration)
    dirSign       = case dir of { DirLeft -> -1.0; DirRight -> 1.0 }
    goombaAccel   = (dirSign * goombaMoveAccel, 0)
    airDrag       = (- (airFrictionCoeff * fst vel), 0)
    totalAccel    = gravityAccel `addVec` goombaAccel `addVec` airDrag
    clampGoombaVelocity (vx, vy) =
      let vx' = max (-goombaWalkSpeed) (min goombaWalkSpeed vx)
      in (vx', vy)
    flipDir DirLeft  = DirRight
    flipDir DirRight = DirLeft

-- After entities update, push overlapping entities (goomba/koopa) apart symmetrically
resolveInterEnemyOverlaps :: GameState -> GameState
resolveInterEnemyOverlaps gs@GameState { entities = es } =
  gs { entities = resolveAll es }
  where
    -- Do a couple of passes for stability
    resolveAll :: [Entity] -> [Entity]
    resolveAll = (!! 2) . iterate resolvePass

    resolvePass :: [Entity] -> [Entity]
    resolvePass = separatePairs 0

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

    -- World blockers (tiles) to prevent pushing enemies into the level
    blockers :: [Collider]
    blockers = colliders (world gs)

    -- Try to move an enemy by a separation vector. If that would collide with the level, cancel the push and keep its original position.
    moveBy :: Vector -> Entity -> Entity
    moveBy (dx, dy) e = case e of
      EGoomba eid g ->
        let oldPos  = goombaPos g
            newPos  = addVecToPoint oldPos (dx, dy)
            allowMove = case goombaColSpec g of
              Nothing   -> True
              Just spec ->
                let testCol = specToCollider newPos (CTEntity eid) spec
                in not (any (collides testCol) blockers)
            (finalPos, finalDir)
              | allowMove =
                  let nd | dx > 0    = DirRight
                         | dx < 0    = DirLeft
                         | otherwise = goombaDir g
                  in (newPos, nd)
              | otherwise = (oldPos, goombaDir g)
        in EGoomba eid g { goombaPos = finalPos, goombaDir = finalDir }
      EKoopa  eid k ->
        let oldPos  = koopaPos k
            newPos  = addVecToPoint oldPos (dx, dy)
            allowMove = case koopaColSpec k of
              Nothing   -> True
              Just spec ->
                let testCol = specToCollider newPos (CTEntity eid) spec
                in not (any (collides testCol) blockers)
            (finalPos, finalDir)
              | allowMove =
                  let nd | dx > 0    = DirRight
                         | dx < 0    = DirLeft
                         | otherwise = koopaDir k
                  in (newPos, nd)
              | otherwise = (oldPos, koopaDir k)
        in EKoopa  eid k { koopaPos  = finalPos, koopaDir = finalDir }
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
