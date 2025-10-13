module Model where

import Graphics.Gloss
import qualified Data.Map as Map

-- GameState
data GameState = GameState
  {
    world      :: World,
    entities   :: [Entity],
    tileMap    :: TileMap,
    tileSize   :: Int,
    frameCount :: Int,
    frameTime  :: Int,
    paused     :: Bool
  } deriving (Show)


-- Level geometry
data World = World 
  {
    grid   :: [[Tile]],
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
    health       :: Int
  } deriving (Show)

data Goomba = Goomba
  {
    goombaPos :: Point,
    goombaVel :: Vector,
    goombaDir :: MoveDir
  } deriving (Show)

data Koopa = Koopa
  {
    koopaPos  :: Point,
    koopaVel  :: Vector,
    koopaDir  :: MoveDir
  } deriving (Show)


type Animation = [Picture]
data MoveDir = Left | Right
  deriving (Eq, Show)