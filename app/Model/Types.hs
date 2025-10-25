module Model.Types where
import Graphics.Gloss
import qualified Data.Map as Map

-- GameState
data GameState = GameState
  {
    world        :: World,
    player       :: Player,
    entities     :: [Entity],
    tileMap      :: TileMap,
    tileZoom     :: Float,
    screenSize   :: (Int, Int),
    frameCount   :: Int,
    frameTime    :: Int,
    paused       :: Bool,
    debugMode    :: Bool,
    pendingJump  :: Bool,
    jumpHeld     :: Bool,
    sprintHeld   :: Bool,
    moveLeftHeld :: Bool,
    moveRightHeld :: Bool
  } deriving (Show)

data MenuState = MenuState
  { menuTileMap      :: TileMap
  , menuPlayerSprite :: Picture
  , menuDebugMode    :: Bool
  , menuScreenSize   :: (Int, Int)
  }

data AppState
  = Menu MenuState
  | Playing GameState

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
  = EGoomba Int Goomba
  | EKoopa Int Koopa
  | EPowerup Int
  | EMovingPlatform Int
  deriving (Eq, Show)

data Player = Player
  {
    playerPos    :: Point,
    playerVel    :: Vector,
    onGround     :: Bool,
    playerSprite :: Animation,
    health       :: Int,
    playerColliderSpec :: Maybe ColliderSpec,
    playerJumpTime :: Float,
    playerJumpDir  :: Vector,
    playerSlide :: Maybe Vector,
    playerAccelTime :: Float,
    playerAccelDir  :: Float,
    playerAccelSprint :: Bool,
    playerCollisions :: [CollisionEvent]
  } deriving (Eq, Show)

data Goomba = Goomba
  {
    goombaPos :: Point,
    goombaVel :: Vector,
    goombaDir :: MoveDir,
    goombaColliderSpec :: Maybe ColliderSpec,
    goombaOnGround :: Bool,
    goombaCollisions :: [CollisionEvent]
  } deriving (Eq, Show)

data Koopa = Koopa
  {
    koopaPos  :: Point,
    koopaVel  :: Vector,
    koopaDir  :: MoveDir,
    koopaColliderSpec :: Maybe ColliderSpec,
    koopaCollisions :: [CollisionEvent]
  } deriving (Eq, Show)


type Animation = [Picture]
data MoveDir = Left | Right
  deriving (Eq, Show)

-- AABB defined by position and size
data Collider = AABB { aPos :: Point, aWidth :: Float, aHeight :: Float, tag :: ColliderTag }
  deriving (Eq)

instance Show Collider where
  show AABB { tag } = show tag


data ColliderTag = None | CTWorld (Int, Int) | CTPlayer Player | CTEntity Entity
  deriving (Eq)

instance Show ColliderTag where
  show None             = "Untagged collider"
  show (CTWorld (x, y)) = "World collider at (" ++ show x ++ ", " ++ show y ++ ")"
  show (CTPlayer _)     = "Player colider"
  show (CTEntity (EGoomba gId _)) = "Goomba colider {id: " ++ show gId ++ " }"
  show (CTEntity (EKoopa  kId _)) = "Koopa  colider {id: " ++ show kId ++ " }"
  show (CTEntity _)     = "Entity collider"


data ColliderSpec = ColliderSpec
  { colliderWidth  :: Float
  , colliderHeight :: Float
  , colliderOffset :: Vector
  } deriving (Eq, Show)

-- Data resulting from a collision check
data CollisionFlags = CollisionFlags
  { hitX           :: Bool
  , hitY           :: Bool
  , groundContact  :: Bool
  , contactNormals :: [Vector]
  } deriving (Eq, Show)

data SweepResult
  = SweepClear Point
  | SweepHit
      { safePos    :: Point
      , hitPos     :: Point
      , hitBlocker :: Collider
      }

data Axis = AxisX | AxisY deriving (Eq, Show)

data CollisionEvent = CollisionEvent 
  { colEventTag    :: ColliderTag
  , colEventAxis   :: Axis
  , colEventNormal :: Vector
  } deriving (Eq, Show)

upVector :: Vector
upVector = (0, 1)