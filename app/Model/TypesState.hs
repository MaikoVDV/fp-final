module Model.TypesState where

import Graphics.Gloss
import qualified Data.Map as Map

import Model.Types
import Model.WorldMap

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
  , uiHeartGolden   :: Picture
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
  , currentMapState :: Maybe MapState -- used when level was started from world map
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

-- World map screen state
data MapState = MapState
  { wmWorldMap  :: WorldMap
  , wmCursor    :: NodeId
  , wmMenuState :: MenuState
  , wmAlong     :: Maybe (EdgeId, [Point], NodeId, Float) -- edge, oriented polyline, destination, t ∈ [0,1]
  , wmSpeed     :: Float -- units per second in map-space
  , wmFilePath  :: FilePath -- json file to serialize progress
  }

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