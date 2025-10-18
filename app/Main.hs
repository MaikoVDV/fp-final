module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model
import qualified Model as M
import View
import Controller
import Assets
import qualified Collision as Collisions
import Control.Monad (when)
import System.Environment (getArgs)

repeatTile :: Int -> Tile -> [Tile]
repeatTile = replicate

main :: IO ()
main = do
  args <- getArgs
  tileMap <- loadTileMap
  playerSprite <- loadPlayerSprite

  let debugEnabled = "debug" `elem` args
      tileSize' = 18
      initialPlayer = Player
        { playerPos    = (1, 10)
        , playerVel    = (0, 0)
        , onGround     = False
        , health       = 1
        , playerSprite = [playerSprite]
        , playerColliderSpec = Just (ColliderSpec
            { colliderWidth  = 0.8
            , colliderHeight = 0.85
            , colliderOffset = (0, 0)
            })
        , playerJumpTime = 0
        , playerJumpDir  = (0, 1)
        , playerSlide = Nothing
        , playerAccelTime = 0
        , playerAccelDir  = 0
        , playerAccelSprint = False
        }

  let width :: Int
      width = 30
      emptyRow :: [Tile]
      emptyRow = replicate width Air
      placeRanges :: [Tile] -> [(Int, Int, Tile)] -> [Tile]
      placeRanges row ranges = foldl (\acc (s,e,t) -> take s acc ++ replicate (e - s + 1) t ++ drop (e+1) acc) row ranges

      skyRows = replicate 6 emptyRow
      upperPlatform = placeRanges emptyRow [ (4,6,Crate), (12,14,Crate), (20,23,Crate) ]
      midAirRow     = placeRanges emptyRow [ (8,8,QuestionBlock), (21,21,QuestionBlock) ]
      lowerPlatform = placeRanges emptyRow [ (2,4,Crate), (15,18,Crate), (26,27,Crate) ]
      subGroundRow  = placeRanges (replicate width Grass) [ (0,2,Crate), (18,20,Crate) ]
      groundRow     = replicate width Grass
      grid = skyRows ++ [upperPlatform, midAirRow, lowerPlatform, subGroundRow, groundRow]

  let state = GameState {
    world     = World
      {
        grid = grid,
        colliders = Collisions.generateCollidersForWorld grid,
        slopes = []
      },
    player    = initialPlayer,
    entities  = [ EGoomba Goomba
      { goombaPos = (fromIntegral (width - 5), 12)
      , goombaVel = (0, 0)
      , goombaDir = M.Left
      , goombaColliderSpec = Just (ColliderSpec { colliderWidth = 0.9, colliderHeight = 0.9, colliderOffset = (0,0) })
      , goombaOnGround = False
      }
    ],
    tileSize  = tileSize',
    tileMap   = tileMap,
    frameCount = 0,
    frameTime  = 30,
    paused     = False,
    debugMode  = debugEnabled,
    pendingJump = False,
    jumpHeld = False,
    sprintHeld = False,
    moveLeftHeld = False,
    moveRightHeld = False
  }

  -- Debug: print generated colliders to check output
  when debugEnabled $ print (colliders (world state))

  playIO
    (InWindow "Maiko & Sam's platformer" (800, 600) (100, 100))
    (makeColorI 135 206 235 255)
    60
    state
    view
    input
    update
