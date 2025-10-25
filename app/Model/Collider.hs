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
playerCollider p@Player { playerPos, playerColliderSpec } =
  specToCollider playerPos (CTPlayer p) <$> playerColliderSpec

-- Returns the Colider of an entity (Handled separately given entity type)
entityCollider :: Entity -> Maybe Collider
entityCollider (EGoomba gId g) = goombaCollider gId g
entityCollider (EKoopa  kId k) = koopaCollider kId k
entityCollider _           = Nothing

goombaCollider :: Int -> Goomba -> Maybe Collider
goombaCollider gId g@Goomba { goombaPos, goombaColliderSpec } =
  specToCollider goombaPos (CTEntity (EGoomba gId g)) <$> goombaColliderSpec

koopaCollider :: Int -> Koopa -> Maybe Collider
koopaCollider kId k@Koopa { koopaPos, koopaColliderSpec } =
  specToCollider koopaPos (CTEntity (EKoopa kId k)) <$> koopaColliderSpec

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
generateCollidersForWorld rows =
  [ AABB (fromIntegral x + 0.5, negate (fromIntegral y) - 0.5) 1 1 (CTWorld (x, y))
  | (y, row) <- zip ([0..] :: [Int]) rows
  , (x, tile) <- zip ([0..] :: [Int]) row
  , isSolid tile
  ]
  where
    isSolid Air = False
    isSolid _   = True

entityCollidersForState :: GameState -> [Collider]
entityCollidersForState GameState { player, entities } =
  catMaybes (playerCollider player : map entityCollider entities)