module Assets where

import qualified Data.Map as Map

import Model
import Graphics.Gloss

loadTileMap :: IO TileMap
loadTileMap = do
  grassPic         <- loadBMP "assets/tiles/grass.bmp"
  cratePic         <- loadBMP "assets/tiles/crate.bmp"
  questionBlockPic <- loadBMP "assets/tiles/question_block.bmp"
  return $ Map.fromList
    [
      (Grass,         grassPic),
      (Crate,         cratePic),
      (QuestionBlock, questionBlockPic)
    ]

loadPlayerSprite :: IO Picture
loadPlayerSprite = loadBMP "assets/player.bmp"