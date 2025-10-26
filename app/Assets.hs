module Assets where

import qualified Data.Map as Map

import Model.Types
import Graphics.Gloss

-- Load tile map from assets

loadTileMap :: IO TileMap
loadTileMap = do
  grassPic         <- loadBMP "assets/tiles/grass.bmp"
  cratePic         <- loadBMP "assets/tiles/crate.bmp"
  metalBoxPic      <- loadBMP "assets/tiles/metal_box.bmp"
  questionBlockFullPic <- loadBMP "assets/tiles/question_block_full.bmp"
  questionBlockEmptyPic <- loadBMP "assets/tiles/question_block_empty.bmp"
  return $ Map.fromList
    [
      (Grass,         grassPic),
      (Crate,         cratePic),
      (MetalBox,      metalBoxPic),
      (QuestionBlockFull, questionBlockFullPic),
      (QuestionBlockEmpty, questionBlockEmptyPic)
    ]

loadAnimMap :: IO AnimMap
loadAnimMap = do
  goombaAnim  <- loadGoombaAnimation
  powerupAnim <- loadPowerupAnimation

  return $ Map.fromList
    [ (TGoomba, goombaAnim)
    , (TPowerup, powerupAnim)
    ]

loadPlayerAnimation :: IO Animation
loadPlayerAnimation = sequence 
  [ loadBMP "assets/entities/player_0.bmp"
  , loadBMP "assets/entities/player_1.bmp"
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