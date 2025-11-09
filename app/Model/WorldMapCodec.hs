{-# LANGUAGE OverloadedStrings #-}

module Model.WorldMapCodec
  ( loadWorldMapFile
  , saveWorldMapFile
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Aeson
import qualified Data.Text as T
import Control.Applicative ((<|>))

import Model.WorldMap

-- Point as [x,y]
instance ToJSON NodeId where
  toJSON (NodeId i) = Number (fromIntegral i)
instance FromJSON NodeId where
  parseJSON = withScientific "NodeId" (\n -> pure (NodeId (round n)))

instance ToJSON EdgeId where
  toJSON (EdgeId i) = Number (fromIntegral i)
instance FromJSON EdgeId where
  parseJSON = withScientific "EdgeId" (\n -> pure (EdgeId (round n)))

instance ToJSON LevelRef where
  toJSON (BuiltIn p)  = object ["builtIn"  .= p]
  toJSON (External p) = object ["external" .= p]
instance FromJSON LevelRef where
  parseJSON = withObject "LevelRef" $ \o ->
    (BuiltIn  <$> o .: "builtIn") <|>
    (External <$> o .: "external")

instance ToJSON NodeType where
  toJSON t = String $ case t of
    Level -> "Level"; Hub -> "Hub"; Boss -> "Boss"; Warp -> "Warp"
instance FromJSON NodeType where
  parseJSON = withText "NodeType" $ \t -> case t of
    "Level" -> pure Level; "Hub" -> pure Hub; "Boss" -> pure Boss; "Warp" -> pure Warp
    _ -> fail "Invalid NodeType"

instance ToJSON NodeState where
  toJSON s = String $ case s of
    Locked -> "Locked"; Unlocked -> "Unlocked"; Completed -> "Completed"
instance FromJSON NodeState where
  parseJSON = withText "NodeState" $ \t -> case t of
    "Locked" -> pure Locked; "Unlocked" -> pure Unlocked; "Completed" -> pure Completed
    _ -> fail "Invalid NodeState"

instance ToJSON EdgeDir where
  toJSON d = String $ case d of
    Undirected -> "Undirected"; AtoB -> "AtoB"; BtoA -> "BtoA"; Both -> "Both"
instance FromJSON EdgeDir where
  parseJSON = withText "EdgeDir" $ \t -> case t of
    "Undirected" -> pure Undirected; "AtoB" -> pure AtoB; "BtoA" -> pure BtoA; "Both" -> pure Both
    _ -> fail "Invalid EdgeDir"

-- Point uses Aeson tuple instance (encoded as [x,y])

instance ToJSON MapNode where
  toJSON MapNode { nodeId, title, levelRef, pos, nodeType, nodeState } =
    object [ "id" .= nodeId
           , "title" .= title
           , "level" .= levelRef
           , "pos" .= pos
           , "type" .= nodeType
           , "state" .= nodeState
           ]
instance FromJSON MapNode where
  parseJSON = withObject "MapNode" $ \o ->
    MapNode <$> o .: "id"
            <*> o .: "title"
            <*> o .:? "level"
            <*> o .: "pos"
            <*> o .: "type"
            <*> o .: "state"

instance ToJSON PathShape where
  toJSON Straight = object ["type" .= String "Straight"]
  toJSON (Polyline ps) = object ["type" .= String "Polyline", "points" .= ps]
  toJSON (Bezier p0 p1 p2 p3) = object ["type" .= String "Bezier", "p0" .= p0, "p1" .= p1, "p2" .= p2, "p3" .= p3]
  toJSON (CatmullRom ps) = object ["type" .= String "CatmullRom", "points" .= ps]
instance FromJSON PathShape where
  parseJSON = withObject "PathShape" $ \o -> do
    ty <- o .: "type"
    case (ty :: T.Text) of
      "Straight"   -> pure Straight
      "Polyline"   -> Polyline   <$> o .: "points"
      "Bezier"     -> Bezier     <$> o .: "p0" <*> o .: "p1" <*> o .: "p2" <*> o .: "p3"
      "CatmullRom" -> CatmullRom <$> o .: "points"
      _ -> fail "Invalid PathShape"

instance ToJSON Edge where
  toJSON Edge { edgeId, a, b, dir, shape, unlocked } =
    object [ "id" .= edgeId
           , "a" .= a
           , "b" .= b
           , "dir" .= dir
           , "shape" .= shape
           , "unlocked" .= unlocked
           ]
instance FromJSON Edge where
  parseJSON = withObject "Edge" $ \o ->
    Edge <$> o .: "id"
         <*> o .: "a"
         <*> o .: "b"
         <*> o .: "dir"
         <*> o .: "shape"
         <*> o .: "unlocked"

instance ToJSON WorldMap where
  toJSON WorldMap { nodes, edges } = object [ "nodes" .= nodes, "edges" .= edges ]
instance FromJSON WorldMap where
  parseJSON = withObject "WorldMap" $ \o ->
    WorldMap <$> o .: "nodes" <*> o .: "edges"

loadWorldMapFile :: FilePath -> IO (Either String WorldMap)
loadWorldMapFile fp = eitherDecode <$> BL.readFile fp

-- Save with a compact, human-friendly layout:
-- - Top-level object pretty
-- - Each node/edge serialized as single-line JSON
-- - Arrays are line-broken with one element per line
saveWorldMapFile :: FilePath -> WorldMap -> IO ()
saveWorldMapFile fp wm = BL.writeFile fp (renderOneLineArrays wm)

renderOneLineArrays :: WorldMap -> BL.ByteString
renderOneLineArrays wm =
  let oneLine a = encode a  -- Aeson encodes compact single-line JSON
      nl = BL8.pack "\n"
      indent n = BL8.pack (replicate n ' ')
      joinWith sep = BL.intercalate sep
      nodesLines = [ indent 4 <> oneLine n | n <- nodes wm ]
      edgesLines = [ indent 4 <> oneLine e | e <- edges wm ]
  in mconcat
      [ BL8.pack "{\n"
      , indent 2, BL8.pack "\"nodes\": [\n"
      , joinWith (BL8.pack ",\n") nodesLines, nl
      , indent 2, BL8.pack "],\n"
      , indent 2, BL8.pack "\"edges\": [\n"
      , joinWith (BL8.pack ",\n") edgesLines, nl
      , indent 2, BL8.pack "]\n"
      , BL8.pack "}\n"
      ]
