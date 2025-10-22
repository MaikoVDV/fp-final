module InitialState
  ( buildInitialGameState
  , baseTilePixelSizeForScreen
  , targetTilesHorizontal
  ) where

import Graphics.Gloss (Picture)

import Model
import qualified Model as M
import qualified Collision as Collisions

targetTilesHorizontal :: Float
targetTilesHorizontal = 20

tilePixelSizeScale :: Float
tilePixelSizeScale = 0.9

baseTilePixelSizeForScreen :: (Int, Int) -> Float
baseTilePixelSizeForScreen (screenWidth, screenHeight) =
  let aspect = fromIntegral screenWidth / max 1 (fromIntegral screenHeight)
  in (fromIntegral screenHeight * aspect / targetTilesHorizontal) * tilePixelSizeScale

buildInitialGameState :: Bool -> TileMap -> Picture -> (Int, Int) -> GameState
buildInitialGameState debugEnabled tileMap playerSpriteImage screenDims =
  let initialPlayer = Player
        { playerPos    = (1, 0)
        , playerVel    = (0, 0)
        , onGround     = False
        , health       = 1
        , playerSprite = [playerSpriteImage]
        , playerColliderSpec = Just ColliderSpec
            { colliderWidth  = 0.6
            , colliderHeight = 0.75
            , colliderOffset = (0, 0)
            }
        , playerJumpTime = 0
        , playerJumpDir  = (0, 1)
        , playerSlide = Nothing
        , playerAccelTime = 0
        , playerAccelDir  = 0
        , playerAccelSprint = False
        }

      width :: Int
      width = 30
      emptyRow :: [Tile]
      emptyRow = replicate width Air
      placeRanges :: [Tile] -> [(Int, Int, Tile)] -> [Tile]
      placeRanges row ranges =
        foldl
          (\acc (s, e, t) -> take s acc ++ replicate (e - s + 1) t ++ drop (e + 1) acc)
          row
          ranges

      skyRows = replicate 6 emptyRow
      upperPlatform = placeRanges emptyRow [ (4,6,Crate), (12,14,Crate), (20,23,Crate) ]
      midAirRow     = placeRanges emptyRow [ (8,8,QuestionBlock), (21,21,QuestionBlock) ]
      lowerPlatform = placeRanges emptyRow [ (2,4,Crate), (15,18,Crate), (26,27,Crate) ]
      subGroundRow  = placeRanges (replicate width Grass) [ (0,2,Crate), (18,20,Crate) ]
      groundRow     = replicate width Grass
      grid = skyRows ++ [upperPlatform, midAirRow, lowerPlatform, subGroundRow, groundRow]

      worldState =
        World
          { grid = grid
          , colliders = Collisions.generateCollidersForWorld grid
          , slopes = []
          }

      goomba = EGoomba Goomba
        { goombaPos = (fromIntegral (width - 5), 12)
        , goombaVel = (0, 0)
        , goombaDir = M.Left
        , goombaColliderSpec = Just ColliderSpec
            { colliderWidth = 0.9
            , colliderHeight = 0.9
            , colliderOffset = (0, 0)
            }
      , goombaOnGround = False
      }
  in GameState
      { world = worldState
      , player = initialPlayer
      , entities = [goomba]
      , tileZoom = 1.0
      , screenSize = screenDims
      , tileMap = tileMap
      , frameCount = 0
      , frameTime = 30
      , paused = False
      , debugMode = debugEnabled
      , pendingJump = False
      , jumpHeld = False
      , sprintHeld = False
      , moveLeftHeld = False
      , moveRightHeld = False
      }
