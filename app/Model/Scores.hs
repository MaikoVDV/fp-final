{-# LANGUAGE DeriveGeneric #-}
module Model.Scores where

import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import Model.Config (defaultLives)

data Scores = Scores
  { lives :: Int
  } deriving (Show, Generic)

instance FromJSON Scores
instance ToJSON Scores

scoresPath :: FilePath
scoresPath = "scores.json"

loadLives :: IO Int
loadLives = do
  exists <- doesFileExist scoresPath
  if not exists
    then return defaultLives
    else do
      bs <- BL.readFile scoresPath
      case decode bs :: Maybe Scores of
        Just s  -> return (max 0 (lives s))
        Nothing -> return defaultLives

saveLives :: Int -> IO ()
saveLives n = BL.writeFile scoresPath (encode (Scores (max 0 n)))
