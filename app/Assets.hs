module Assets where

import qualified Data.Map as Map

import Model.Types
import Graphics.Gloss

-- Load tile map from assets

loadTileMap :: IO TileMap
loadTileMap = do
  grassPic              <- loadBMP "assets/tiles/grass.bmp"
  cratePic              <- loadBMP "assets/tiles/crate.bmp"
  metalBoxPic           <- loadBMP "assets/tiles/metal_box.bmp"
  questionBlockFullPic  <- loadBMP "assets/tiles/question_block_full.bmp"
  questionBlockEmptyPic <- loadBMP "assets/tiles/question_block_empty.bmp"
  flagPic               <- loadBMP "assets/tiles/flag_0.bmp"
  spikesPic             <- loadBMP "assets/tiles/spikes.bmp"
  return $ Map.fromList
    [ (Grass,              grassPic)
    , (Crate,              cratePic)
    , (MetalBox,           metalBoxPic)
    , (QuestionBlockFull,  questionBlockFullPic)
    , (QuestionBlockEmpty, questionBlockEmptyPic)
    , (Flag,               flagPic)
    , (Spikes,             spikesPic)
    ]

loadAnimMap :: IO AnimMap
loadAnimMap = do
  goombaAnim  <- loadGoombaAnimation
  powerupAnim <- loadPowerupAnimation

  return $ Map.fromList
    [ (TGoomba, goombaAnim)
    , (TPowerup, powerupAnim)
    ]

loadPlayerAnimation :: IO [Animation]
loadPlayerAnimation = sequence 
  [ sequence 
    [ loadBMP "assets/entities/player_1hp_0.bmp"
    , loadBMP "assets/entities/player_1hp_1.bmp"
    ]
  , sequence 
    [ loadBMP "assets/entities/player_2hp_0.bmp"
    , loadBMP "assets/entities/player_2hp_1.bmp"
    ]
  , sequence 
    [ loadBMP "assets/entities/player_3hp_0.bmp"
    , loadBMP "assets/entities/player_3hp_1.bmp"
    ]
  ]

loadGoombaAnimation :: IO Animation
loadGoombaAnimation = sequence 
  [ loadBMP "assets/entities/goomba_0.bmp"
  , loadBMP "assets/entities/goomba_1.bmp"
  , loadBMP "assets/entities/goomba_2.bmp"
  ]
loadPowerupAnimation :: IO Animation
loadPowerupAnimation = sequence 
  [ loadBMP "assets/entities/burger.bmp"
  ]