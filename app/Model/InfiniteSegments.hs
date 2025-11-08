{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.InfiniteSegments
  ( SegmentMeta(..)
  , segmentsDirectory
  , segmentMetaSuffix
  , loadSegmentMetas
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.List (isSuffixOf)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))

segmentsDirectory :: FilePath
segmentsDirectory = "Infinite mode segments"

segmentMetaSuffix :: String
segmentMetaSuffix = ".segment.json"

data SegmentMeta = SegmentMeta
  { segmentName :: String
  , levelPath   :: FilePath
  , levelFile   :: FilePath
  , startHeight :: Int
  , endHeight   :: Int
  } deriving (Eq, Show, Generic)

instance FromJSON SegmentMeta
instance ToJSON SegmentMeta

loadSegmentMetas :: IO [SegmentMeta]
loadSegmentMetas = do
  exists <- doesDirectoryExist segmentsDirectory
  if not exists
    then return []
    else do
      files <- listDirectory segmentsDirectory
      let metaFiles = [ segmentsDirectory </> f | f <- files, segmentMetaSuffix `isSuffixOf` f ]
      concat <$> mapM loadMeta metaFiles
  where
    loadMeta path = do
      bs <- BL.readFile path
      case eitherDecode bs of
        Left err -> do
          putStrLn ("Failed to read segment metadata " ++ path ++ ": " ++ err)
          return []
        Right meta -> return [meta]
