module Model.Types where
import Graphics.Gloss
import qualified Data.Map as Map
import Model.WorldMap (WorldMap, NodeId, EdgeId)
import Model.InfiniteSegments (SegmentMeta)

-- GameState
data GameState = GameState
  { world           :: World
  , player          :: Player
  , entities        :: [Entity]
  , entityIdCounter :: Int
  , tileMap         :: TileMap
  , animMap         :: AnimMap
  , uiHeartFull     :: Picture
  , uiHeartHalf     :: Picture
  , uiHeartEmpty    :: Picture
  , uiCounters      :: Map.Map Char Picture
  , playerLives     :: Int
  , coinsCollected  :: Int
  , tileZoom        :: Float
  , screenSize      :: (Int, Int)
  , frameCount      :: Int
  , paused          :: Bool
  , debugMode       :: Bool
  , pendingJump     :: Bool
  , jumpHeld        :: Bool
  , sprintHeld      :: Bool
  , menuState       :: MenuState
  , nextState       :: NextState
  , currentMapState :: Maybe MapState -- when level started from world map
  , infiniteState   :: Maybe InfiniteRunState
  }

data MenuPage = MainMenu | CustomLevels | BuilderSelect | BuilderName deriving (Eq, Show)

data MenuState = MenuState
  { menuPlayerAnim   :: [Animation]
  , menuDebugMode    :: Bool
  , menuScreenSize   :: (Int, Int)
  , menuFocus        :: Int           -- focused index (context-sensitive)
  , menuPage         :: MenuPage      -- which menu page to render
  , menuCustomFiles  :: [FilePath]    -- cached custom level file list
  , menuInput        :: String        -- generic text input buffer (e.g., new level name)
  }

data AppState
  = Menu MenuState
  | Playing GameState
  | Building BuilderState
  | WorldMapScreen MapState

data NextState
  = NPlaying
  | NDeath
  | NMenu
  | NFinishLevel

data InfiniteRunState = InfiniteRunState
  { infSegments      :: [ActiveSegment]
  , infSegmentPool   :: [SegmentMeta]
  , infSegmentsAhead :: Int
  } deriving (Eq, Show)

data ActiveSegment = ActiveSegment
  { activeMeta  :: SegmentMeta
  , activeStartX :: Int
  , activeWidth :: Int
  } deriving (Eq, Show)

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
  | Earth
  | Earth2
  | Crate
  | MetalBox
  | QuestionBlockFull
  | QuestionBlockEmpty
  | Flag
  | Spikes
  deriving (Eq, Ord, Show)

data SlopeData = SlopeData
  {
    start :: Point,
    end   :: Point
  } deriving (Eq, Show)

type TileMap = Map.Map Tile Picture
type AnimMap = Map.Map EntityType Animation

-- Level Builder
data BuilderState = BuilderState
  { builderWorld      :: World
  , builderTileMap    :: TileMap
  , builderAnimMap    :: AnimMap
  , builderTileZoom   :: Float
  , builderScreenSize :: (Int, Int)
  , builderDebugMode  :: Bool
  , builderBrush      :: Tile
  , builderBrushMode  :: BrushMode
  , builderDirty      :: Bool
  , builderConfirmLeave :: Bool
  , builderPaletteTab :: PaletteTab
  , builderEnemySel   :: EnemySel
  , builderEntities   :: [Entity]
  , builderCam        :: (Float, Float)
  , builderPanning    :: Bool
  , builderLastMouse  :: (Float, Float)
  , builderLMBHeld    :: Bool
  , builderLastPaint  :: Maybe (Int, Int)
  , builderFilePath   :: Maybe FilePath
  }

data BrushMode = BrushNormal | BrushGrassColumn | BrushEraser deriving (Eq, Show)

data PaletteTab = TabBlocks | TabEnemies deriving (Eq, Show)

data EnemySel = EnemyGoomba | EnemyCoin | EnemyEraser deriving (Eq, Show)

-- Entities
data Entity
  = EGoomba Int Goomba
  | EKoopa Int Koopa
  | EPowerup Int Powerup
  | ECoin Int Coin
  | EPlatform Int
  deriving (Eq, Show)

-- Used as a bridge between entity data and animations
data EntityType
  = TGoomba
  | TKoopa
  | TPowerup
  | TCoin
  | TPlatform
  deriving (Eq, Ord, Show)

instance Ord Entity where
  e1 <= e2 = getEntityId e1 <= getEntityId e2

-- Function must be defined in this file because the instance of Ord for Entity uses it
getEntityId :: Entity -> Int
getEntityId (EGoomba   gId  _) = gId
getEntityId (EKoopa    kId  _) = kId
getEntityId (EPowerup  puId _) = puId
getEntityId (ECoin     cId  _) = cId
getEntityId (EPlatform pfId  ) = pfId


data Player = Player
  { playerPos    :: Point
  , playerVel    :: Vector
  , playerAnim   :: [Animation]
  , onGround     :: Bool
  , health       :: Int
  , playerColSpec :: Maybe ColliderSpec
  , playerJumpTime :: Float
  , playerJumpDir  :: Vector
  , playerSlide :: Maybe Vector
  , playerAccelTime :: Float
  , playerAccelDir  :: Float
  , playerAccelSprint :: Bool
  , playerCollisions :: [CollisionEvent]
  , moveLeftHeld    :: Bool
  , moveRightHeld   :: Bool
  , lastMoveDir     :: Float
  , jumpsLeft       :: Int
  , stompJumpTimeLeft :: Float
  , playerAnimClock :: Float
  , invulnTimeLeft  :: Float
  } deriving (Eq, Show)

data Goomba = Goomba
  { goombaPos :: Point
  , goombaVel :: Vector
  , goombaDir :: MoveDir
  , goombaColSpec :: Maybe ColliderSpec
  , goombaOnGround :: Bool
  , goombaCollisions :: [CollisionEvent]
  , goombaMode :: GoombaMode
  } deriving (Eq, Show)

-- Goomba behavior mode
--  - GWalking: normal walking behavior
--  - GShelled t: in-shell state for t seconds (no horizontal movement)
data GoombaMode = GWalking | GShelled Float
  deriving (Eq, Show)

data Koopa = Koopa
  { koopaPos  :: Point
  , koopaVel  :: Vector
  , koopaDir  :: MoveDir
  , koopaColSpec :: Maybe ColliderSpec
  , koopaCollisions :: [CollisionEvent]
  } deriving (Eq, Show)

data Powerup = Powerup
  { powerupPos  :: Point
  , powerupVel  :: Vector
  , powerupDir  :: MoveDir
  , powerupColSpec :: Maybe ColliderSpec
  , powerupCollisions :: [CollisionEvent]
  } deriving (Eq, Show)

data Coin = Coin
  { coinPos  :: Point
  , coinColSpec :: Maybe ColliderSpec
  } deriving (Eq, Show)

type Animation = [Picture]
data MoveDir = Left | Right
  deriving (Eq, Show)

-- World map screen state
data MapState = MapState
  { wmWorldMap  :: WorldMap
  , wmCursor    :: NodeId
  , wmMenuState :: MenuState
  , wmAlong     :: Maybe (EdgeId, [Point], NodeId, Float) -- edge, oriented polyline, destination, t ∈ [0,1]
  , wmSpeed     :: Float -- units per second in map-space
  , wmFilePath  :: FilePath -- source JSON to persist progress
  }

-- AABB defined by position and size
data Collider = AABB { aPos :: Point, aWidth :: Float, aHeight :: Float, tag :: ColliderTag }
  deriving (Eq)

instance Show Collider where
  show AABB { tag } = show tag


data ColliderTag = None | CTWorld (Int, Int) | CTWorldSpan Int Int Int | CTPlayer Player | CTEntity Int
  deriving (Eq)

instance Show ColliderTag where
  show None             = "Untagged collider"
  show (CTWorld (x, y)) = "World collider at (" ++ show x ++ ", " ++ show y ++ ")"
  show (CTWorldSpan y s e) = "World span at row " ++ show y ++ ": [" ++ show s ++ ", " ++ show e ++ "]"
  show (CTPlayer _)     = "Player colider"
  show (CTEntity gId)   = "Entity colider {id: " ++ show gId ++ " }"


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
