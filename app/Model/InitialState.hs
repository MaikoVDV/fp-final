module Model.InitialState where

import Model.Types
import qualified Model.Types as Types
import Model.Collider
import Assets 
import Model.Entity (defaultGoomba, defaultPlayer)

targetTilesHorizontal :: Float
targetTilesHorizontal = 20

tilePixelSizeScale :: Float
tilePixelSizeScale = 0.9

baseTilePixelSizeForScreen :: (Int, Int) -> Float
baseTilePixelSizeForScreen (screenWidth, screenHeight) =
  let aspect = fromIntegral screenWidth / max 1 (fromIntegral screenHeight)
  in (fromIntegral screenHeight * aspect / targetTilesHorizontal) * tilePixelSizeScale

buildInitialGameState :: Bool -> (Int, Int) -> IO GameState
buildInitialGameState debugEnabled screenDims = do
    tileMap         <- loadTileMap
    animMap         <- loadAnimMap
    playerAnimation <- loadPlayerAnimation
    let initialPlayer = defaultPlayer {
      playerPos = (1, 0),
      playerAnim = playerAnimation
    }
        width = 30
        emptyRow = replicate width Air

        skyRows = replicate 6 emptyRow
        topBoxes      = placeRanges emptyRow [ (4,6,QuestionBlockFull) ]
        upperPlatform = placeRanges emptyRow [ (3,3,MetalBox),(4,6,Crate),(7,7,MetalBox),(11,11,MetalBox),(12,14,Crate),(15,15,MetalBox),(19,19,MetalBox),(20,23,Crate),(24,24,MetalBox) ]
        midAirRow     = placeRanges emptyRow [ (8,8,QuestionBlockFull) ]
        lowerPlatform = placeRanges emptyRow [ (2,4,Crate), (15,18,Crate), (26,27,Crate) ]
        subGroundRow  = placeRanges (replicate width Grass) [ (0,2,Crate), (18,20,Crate) ]
        groundRow     = replicate width Grass
        grid = skyRows ++ [topBoxes, emptyRow, emptyRow, upperPlatform, midAirRow, emptyRow, lowerPlatform, subGroundRow, groundRow]

        worldState =
          World
            { grid = grid
            , colliders = generateCollidersForWorld grid
            , slopes = []
            }

        goomba0 = EGoomba 0 defaultGoomba
          { goombaPos = (fromIntegral (width - 5), -5)
          , goombaDir = Types.Left
          }
        goomba1 = EGoomba 1 defaultGoomba
          { goombaPos = (fromIntegral (width - 10), -5)
          , goombaDir = Types.Left
          }
        placeRanges :: [Tile] -> [(Int, Int, Tile)] -> [Tile]
        placeRanges = foldl
            (\acc (s, e, t) -> take s acc ++ replicate (e - s + 1) t ++ drop (e + 1) acc)
    return $ GameState
      { world = worldState
      , player = initialPlayer
      , entities = [goomba0, goomba1]
      , entityIdCounter = 2
      , tileZoom = 1.0
      , screenSize = screenDims
      , tileMap = tileMap
      , animMap = animMap
      , frameCount = 0
      , paused = False
      , debugMode = debugEnabled
      , pendingJump = False
      , jumpHeld = False
      , sprintHeld = False
      }
