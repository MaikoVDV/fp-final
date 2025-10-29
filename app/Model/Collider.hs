module Model.Collider where
import Model.Types

import Graphics.Gloss
import Data.Maybe

-- Converts a ColliderSpec to a Tagged Collider
specToCollider :: Point -> ColliderTag -> ColliderSpec -> Collider
specToCollider (cx, cy) colliderTag ColliderSpec { colliderWidth, colliderHeight, colliderOffset = (ox, oy) } =
  AABB (cx + ox, cy + oy) colliderWidth colliderHeight colliderTag

-- Returns the player's Collider
playerCollider :: Player -> Maybe Collider
playerCollider p@Player { playerPos, playerColSpec } =
  specToCollider playerPos (CTPlayer p) <$> playerColSpec

-- Returns the Colider of an entity (Handled separately given entity type)
entityCollider :: Entity -> Maybe Collider
entityCollider (EGoomba  gId  g)  = goombaCollider  gId g
entityCollider (EKoopa   kId  k)  = koopaCollider   kId k
entityCollider (EPowerup puId pu) = powerupCollider puId pu
entityCollider _                  = Nothing

goombaCollider :: Int -> Goomba -> Maybe Collider
goombaCollider gId Goomba { goombaPos, goombaColSpec } =
  specToCollider goombaPos (CTEntity gId) <$> goombaColSpec

koopaCollider :: Int -> Koopa -> Maybe Collider
koopaCollider kId Koopa { koopaPos, koopaColSpec } =
  specToCollider koopaPos (CTEntity kId) <$> koopaColSpec

powerupCollider :: Int -> Powerup -> Maybe Collider
powerupCollider puId Powerup { powerupPos, powerupColSpec } =
  specToCollider powerupPos (CTEntity puId) <$> powerupColSpec

-- simple AABB vs AABB overlap test (treat widths/heights as full sizes)
collides :: Collider -> Collider -> Bool
collides (AABB (x1,y1) w1 h1 _) (AABB (x2,y2) w2 h2 _) =
  abs (x1 - x2) * 2 < (w1 + w2) && abs (y1 - y2) * 2 < (h1 + h2)

noCollisionFlags :: CollisionFlags
noCollisionFlags = CollisionFlags False False False []

combineFlags :: CollisionFlags -> CollisionFlags -> CollisionFlags
combineFlags a b = CollisionFlags
  { hitX           = hitX a || hitX b
  , hitY           = hitY a || hitY b
  , groundContact  = groundContact a || groundContact b
  , contactNormals = contactNormals a ++ contactNormals b
  }

-- Nullifies velocity along x or y if there was a collision along that axis
applyCollisionFlags :: CollisionFlags -> Vector -> Vector
applyCollisionFlags CollisionFlags { hitX, hitY } (vx, vy) =
  let vx' = if hitX then 0 else vx
      vy' = if hitY then 0 else vy
  in (vx', vy')

generateCollidersForWorld :: [[Tile]] -> [Collider]
generateCollidersForWorld rows = concat
  [ mergeRow y row
  | (y, row) <- zip ([0..] :: [Int]) rows
  ]
  where
    isSolid :: Tile -> Bool
    isSolid Air = False
    isSolid _   = True

    mergeRow :: Int -> [Tile] -> [Collider]
    mergeRow y row = go 0 []
      where
        rowLen = length row
        yCenter = negate (fromIntegral y) - 0.5 :: Float

        go :: Int -> [Collider] -> [Collider]
        go x acc
          | x >= rowLen = reverse acc
          | not (isSolid (row !! x)) = go (x + 1) acc
          | otherwise =
              let start = x
                  lenRun = runLength start
                  endIdx = start + lenRun - 1
                  centerX = fromIntegral start + (fromIntegral lenRun) / 2
                  collider = AABB (centerX, yCenter) (fromIntegral lenRun) 1 (CTWorldSpan y start endIdx)
              in go (endIdx + 1) (collider : acc)

        runLength :: Int -> Int
        runLength i = length (takeWhile id (map isSolid (take rowLen (drop i row))))

entityCollidersForState :: GameState -> [Collider]
entityCollidersForState GameState { player, entities } =
  catMaybes (playerCollider player : map entityCollider entities)
