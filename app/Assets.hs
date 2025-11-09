module Assets where

import qualified Data.Map as Map

import Model.Types
import Graphics.Gloss

-- Load tile map from assets
loadTileMap :: IO TileMap
loadTileMap = do
  grassPic              <- loadBMP "assets/tiles/grass.bmp"
  earthPic              <- loadBMP "assets/tiles/earth.bmp"
  earth2Pic             <- loadBMP "assets/tiles/earth2.bmp"
  cratePic              <- loadBMP "assets/tiles/crate.bmp"
  metalBoxPic           <- loadBMP "assets/tiles/metal_box.bmp"
  questionBlockFullPic  <- loadBMP "assets/tiles/question_block_full.bmp"
  questionBlockEmptyPic <- loadBMP "assets/tiles/question_block_empty.bmp"
  flagPic               <- loadBMP "assets/tiles/flag_0.bmp"
  spikesPic             <- loadBMP "assets/tiles/spikes.bmp"
  return $ Map.fromList
    [ (Grass,              grassPic)
    , (Earth,              earthPic)
    , (Earth2,             earth2Pic)
    , (Crate,              cratePic)
    , (MetalBox,           metalBoxPic)
    , (QuestionBlockFull,  questionBlockFullPic)
    , (QuestionBlockEmpty, questionBlockEmptyPic)
    , (Flag,               flagPic)
    , (Spikes,             spikesPic)
    ]

-- Entity and player animations
loadAnimMap :: IO AnimMap
loadAnimMap = do
  goombaAnim  <- loadGoombaAnimation
  powerupAnim <- loadPowerupAnimation
  coinAnim    <- loadCoinAnimation

  return $ Map.fromList
    [ (TGoomba, goombaAnim)
    , (TPowerup, powerupAnim)
    , (TCoin, coinAnim)
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

loadCoinAnimation :: IO Animation
loadCoinAnimation = sequence
  [ loadBMP "assets/tiles/coin_0.bmp"
  , loadBMP "assets/tiles/coin_1.bmp"
  ]

-- UI assets
loadHeartsUI :: IO (Picture, Picture, Picture, Picture)
loadHeartsUI = do
  full  <- loadBMP "assets/ui/Hearts/Heart_Full.bmp"
  half  <- loadBMP "assets/ui/Hearts/Heart_Half.bmp"
  empty <- loadBMP "assets/ui/Hearts/Heart_Empty.bmp"
  golden <- loadBMP "assets/ui/Hearts/Heart_Golden.bmp"
  return (full, half, empty, golden)

loadCountersUI :: IO (Map.Map Char Picture)
loadCountersUI = do
  zero <- loadBMP "assets/ui/Counters/0.bmp"
  one  <- loadBMP "assets/ui/Counters/1.bmp"
  two  <- loadBMP "assets/ui/Counters/2.bmp"
  three<- loadBMP "assets/ui/Counters/3.bmp"
  four <- loadBMP "assets/ui/Counters/4.bmp"
  five <- loadBMP "assets/ui/Counters/5.bmp"
  six  <- loadBMP "assets/ui/Counters/6.bmp"
  seven<- loadBMP "assets/ui/Counters/7.bmp"
  eight<- loadBMP "assets/ui/Counters/8.bmp"
  nine <- loadBMP "assets/ui/Counters/9.bmp"
  xpic <- loadBMP "assets/ui/Counters/x.bmp"
  return $ Map.fromList
    [ ('0', zero), ('1', one), ('2', two), ('3', three), ('4', four)
    , ('5', five), ('6', six), ('7', seven), ('8', eight), ('9', nine)
    , ('x', xpic)
    ]
