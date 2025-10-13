module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Model
import View
import Controller
import Assets

main :: IO ()
main = do
  tileMap <- loadTileMap
  playerSprite <- loadPlayerSprite

  let initialPlayer = Player {
    playerPos    = (18, 18 * 10),
    playerVel    = (0, 0),
    onGround     = False,
    health       = 1,
    playerSprite = [playerSprite]
  }
  let state = GameState {
    world     = World 
      {
        grid = [[Crate, Crate], [Grass, Grass, Grass]],
        slopes = []
      },
    entities  = [EPlayer initialPlayer],
    tileSize  = 18,
    tileMap   = tileMap,
    frameCount = 0,
    frameTime  = 30,
    paused     = False
  }

  playIO
    (InWindow "Maiko & Sam's platformer" (800, 600) (100, 100))
    white
    60
    state
    view
    input
    update
