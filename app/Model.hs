module Model where

import Graphics.Gloss
import qualified Data.Map as Map

-- GameState
data GameState = GameState
  {
    world      :: World,
    player     :: Player,
    entities   :: [Entity],
    tileMap    :: TileMap,
    tileSize   :: Int,
    frameCount :: Int,
    frameTime  :: Int,
    paused     :: Bool,
    debugMode  :: Bool
  } deriving (Show)


-- Level geometry
data World = World
  {
    grid   :: [[Tile]],
    colliders :: [Collider],
    slopes :: [SlopeData]
  } deriving (Eq, Show)

data Tile
  = Air
  | Grass
  | Crate
  | QuestionBlock
  deriving (Eq, Ord, Show)

data SlopeData = SlopeData
  {
    start :: Point,
    end   :: Point
  } deriving (Eq, Show)
  
type TileMap = Map.Map Tile Picture

-- Entities
data Entity
  = EPlayer Player
  | EGoomba Goomba
  | EKoopa Koopa
  | EPowerup
  | EMovingPlatform
  deriving (Show)

data Player = Player 
  {
    playerPos    :: Point,
    playerVel    :: Vector,
    onGround     :: Bool,
    playerSprite :: Animation,
    health       :: Int,
    playerColliderSpec :: Maybe ColliderSpec
  } deriving (Show)

data Goomba = Goomba
  {
    goombaPos :: Point,
    goombaVel :: Vector,
    goombaDir :: MoveDir,
    goombaColliderSpec :: Maybe ColliderSpec
  } deriving (Show)

data Koopa = Koopa
  {
    koopaPos  :: Point,
    koopaVel  :: Vector,
    koopaDir  :: MoveDir,
    koopaColliderSpec :: Maybe ColliderSpec
  } deriving (Show)


type Animation = [Picture]
data MoveDir = Left | Right
  deriving (Eq, Show)


  
-- Axis-aligned bounding box: position + size (half-extents or full size as you prefer)
data Collider = AABB { aPos :: Point, aWidth :: Float, aHeight :: Float }
  deriving (Eq, Show)

data ColliderSpec = ColliderSpec
  { colliderWidth  :: Float
  , colliderHeight :: Float
  , colliderOffset :: Vector
  } deriving (Eq, Show)

specToCollider :: Point -> ColliderSpec -> Collider
specToCollider (cx, cy) ColliderSpec { colliderWidth, colliderHeight, colliderOffset = (ox, oy) } =
  AABB (cx + ox, cy + oy) colliderWidth colliderHeight

playerCollider :: Player -> Maybe Collider
playerCollider Player { playerPos, playerColliderSpec } =
  specToCollider playerPos <$> playerColliderSpec

goombaCollider :: Goomba -> Maybe Collider
goombaCollider Goomba { goombaPos, goombaColliderSpec } =
  specToCollider goombaPos <$> goombaColliderSpec

koopaCollider :: Koopa -> Maybe Collider
koopaCollider Koopa { koopaPos, koopaColliderSpec } =
  specToCollider koopaPos <$> koopaColliderSpec

entityCollider :: Entity -> Maybe Collider
entityCollider (EPlayer p) = playerCollider p
entityCollider (EGoomba g) = goombaCollider g
entityCollider (EKoopa  k) = koopaCollider k
entityCollider _           = Nothing
