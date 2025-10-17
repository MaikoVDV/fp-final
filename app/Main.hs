module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model
import View
import Controller
import Assets
import qualified Collision as Collisions
import Control.Monad (when)
import System.Environment (getArgs)

repeatTile :: Int -> Tile -> [Tile]
repeatTile n tile = replicate n tile

main :: IO ()
main = do
  args <- getArgs
  tileMap <- loadTileMap
  playerSprite <- loadPlayerSprite

  let debugEnabled = any (== "debug") args
      tileSize' = 18
      initialPlayer = Player
        { playerPos    = (1, 10)
        , playerVel    = (0, 0)
        , onGround     = False
        , health       = 1
        , playerSprite = [playerSprite]
        , playerColliderSpec = Just (ColliderSpec
            { colliderWidth  = 1
            , colliderHeight = 1
            , colliderOffset = (0, 0)
            })
        , playerJumpTime = 0
        }

  let groundRow = repeatTile 20 Grass
      grid =
        [ repeatTile 20 Air
        , groundRow
        ]

  let state = GameState {
    world     = World
      {
        grid = grid,
        colliders = Collisions.generateCollidersForWorld grid,
        slopes = []
      },
    player    = initialPlayer,
    entities  = [],
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
    white
    60
    state
    view
    input
    update
