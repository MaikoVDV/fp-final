module Model.WorldMap where

import Graphics.Gloss
import Data.Maybe (mapMaybe)

-- Identifiers
newtype NodeId = NodeId Int deriving (Eq, Ord, Show)
newtype EdgeId = EdgeId Int deriving (Eq, Ord, Show)

-- Where to find the level when entering a node
data LevelRef
  = BuiltIn FilePath    -- e.g. "built-in-levels/world1/1-1.lvl"
  | External FilePath   -- e.g. "levels/my-level.lvl"
  deriving (Eq, Show)

data NodeType = Level | Hub | Boss | Warp deriving (Eq, Show)
data NodeState = Locked | Unlocked | Completed deriving (Eq, Show)

data MapNode = MapNode
  { nodeId    :: NodeId
  , title     :: String
  , levelRef  :: Maybe LevelRef -- Hubs may not correspond to a level
  , pos       :: Point          -- Map-space position
  , nodeType  :: NodeType
  , nodeState :: NodeState
  } deriving (Eq, Show)

data EdgeDir = Undirected | AtoB | BtoA | Both deriving (Eq, Show)

data PathShape
  = Straight                   -- Connect node positions with a straight line
  | Polyline [Point]           -- Absolute points
  | Bezier Point Point Point Point    -- Cubic Bezier via absolute points
  | CatmullRom [Point]               -- Through-points spline
  deriving (Eq, Show)

data Edge = Edge
  { edgeId   :: EdgeId
  , a        :: NodeId
  , b        :: NodeId
  , dir      :: EdgeDir
  , shape    :: PathShape
  , unlocked :: Bool
  } deriving (Eq, Show)

data WorldMap = WorldMap
  { nodes :: [MapNode]
  , edges :: [Edge]
  } deriving (Eq, Show)

-- Queries
nodeById :: WorldMap -> NodeId -> Maybe MapNode
nodeById WorldMap{nodes} nid = go nodes
  where
    go [] = Nothing
    go (n:ns) | nodeId n == nid = Just n
              | otherwise       = go ns

nodePos :: WorldMap -> NodeId -> Maybe Point
nodePos wm nid = pos <$> nodeById wm nid

neighbors :: WorldMap -> NodeId -> [NodeId]
neighbors wm nid = [ other e | e <- edges wm, visible e ]
  where
    visible e = unlocked e && case dir e of
      Undirected -> a e == nid || b e == nid
      Both       -> a e == nid || b e == nid
      AtoB       -> a e == nid
      BtoA       -> b e == nid
    other e = if a e == nid then b e else a e

-- Rendering helpers ---------------------------------------------------------

-- Simple sampling utilities to turn curves into polylines
bezier3 :: Float -> Point -> Point -> Point -> Point -> Point
bezier3 t (x0,y0) (x1,y1) (x2,y2) (x3,y3) = (bx x0 x1 x2 x3, bx y0 y1 y2 y3)
  where
    bx a b c d =
      let u = 1 - t
      in u*u*u*a + 3*u*u*t*b + 3*u*t*t*c + t*t*t*d

sampleBezier :: Int -> (Point,Point,Point,Point) -> [Point]
sampleBezier n (p0,p1,p2,p3) =
  [ bezier3 t p0 p1 p2 p3 | i <- [0..n], let t = fromIntegral i / fromIntegral n ]

catmullRomPoint :: Float -> Point -> Point -> Point -> Point -> Point
catmullRomPoint t (x0,y0) (x1,y1) (x2,y2) (x3,y3) = (sx x0 x1 x2 x3, sx y0 y1 y2 y3)
  where
    t2 = t*t; t3 = t2*t
    sx a b c d = 0.5 * ( (2*b)
       + (-a + c) * t
       + (2*a - 5*b + 4*c - d) * t2
       + (-a + 3*b - 3*c + d) * t3 )

sampleCatmullRom :: Int -> [Point] -> [Point]
sampleCatmullRom n ps
  | length ps < 2 = ps
  | otherwise     = concatMap seg [0 .. length ps - 2]
  where
    at i = ps !! max 0 (min (length ps - 1) i)
    seg i =
      let p0 = at (i-1); p1 = at i; p2 = at (i+1); p3 = at (i+2)
      in [ catmullRomPoint t p0 p1 p2 p3
         | k <- [0..n-1], let t = fromIntegral k / fromIntegral n
         ] ++ [p2 | i == length ps - 2]

polyline :: [Point] -> Picture
polyline []  = blank
polyline [_] = blank
polyline ps  = line ps

edgeToPolyline :: WorldMap -> Edge -> [Point]
edgeToPolyline wm e = case shape e of
  Straight -> case (nodePos wm (a e), nodePos wm (b e)) of
    (Just pa, Just pb) -> [pa, pb]
    _                  -> []
  Polyline ps -> ps
  Bezier p0 p1 p2 p3 -> sampleBezier 24 (p0,p1,p2,p3)
  CatmullRom ps      -> sampleCatmullRom 10 ps

-- Public helpers ------------------------------------------------------------

edgeById :: WorldMap -> EdgeId -> Maybe Edge
edgeById WorldMap{edges} eid = go edges
  where
    go [] = Nothing
    go (e:es) | edgeId e == eid = Just e
              | otherwise       = go es

edgePoints :: WorldMap -> Edge -> [Point]
edgePoints = edgeToPolyline

polylineLength :: [Point] -> Float
polylineLength []       = 0
polylineLength [_]      = 0
polylineLength (p:q:xs) = dist p q + polylineLength (q:xs)
  where
    dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^(2 :: Int) + (y2-y1)^(2 :: Int))

polylineAt :: [Point] -> Float -> Point
polylineAt ps t
  | null ps        = (0,0)
  | length ps == 1 = head ps
  | otherwise      = go segments (u * totalLen)
  where
    segments = zip ps (tail ps)
    totalLen = sum [ d a b | (a,b) <- segments ]
    u        = max 0 (min 1 t)
    d (x1,y1) (x2,y2) = sqrt ((x2-x1)^(2 :: Int) + (y2-y1)^(2 :: Int))
    lerp a b s = a + s * (b - a)
    go [] _ = last ps
    go ((p0@(x0,y0), p1@(x1,y1)):segs) remaining
      | segLen <= 1e-6 = go segs remaining
      | remaining <= segLen =
          let s = remaining / segLen in ( lerp x0 x1 s, lerp y0 y1 s )
      | otherwise = go segs (remaining - segLen)
      where segLen = d p0 p1

-- Return unlocked, direction-allowed options from a node, with oriented polyline
-- (points start at current node and end at neighbor) and the unit direction.
adjacentDirected :: WorldMap -> NodeId -> [(Edge, NodeId, [Point], (Float, Float))]
adjacentDirected wm nid = mapMaybe option (edges wm)
  where
    herePos = nodePos wm nid

    option e
      | not (unlocked e) = Nothing
      | otherwise = case dir e of
          Undirected -> eitherWay e
          Both       -> eitherWay e
          AtoB       -> if a e == nid then forward e else Nothing
          BtoA       -> if b e == nid then forward e else Nothing

    eitherWay e = case () of
      _ | a e == nid -> forward e
        | b e == nid -> forward e
        | otherwise  -> Nothing

    forward e = do
      hp <- herePos
      let nbId = if a e == nid then b e else a e
      nb <- nodePos wm nbId
      let pts0 = edgeToPolyline wm e
          oriented = orient pts0 hp nb
          dirV = unit (dxdy hp nb)
      return (e, nbId, oriented, dirV)

    orient :: [Point] -> Point -> Point -> [Point]
    orient pts start _ =
      case pts of
        (p0:_) | close p0 start -> pts
        _ -> reverse pts

    close (x1,y1) (x2,y2) = (abs (x1 - x2) + abs (y1 - y2)) < 1e-3
    dxdy (x1,y1) (x2,y2) = (x2 - x1, y2 - y1)
    unit (x,y) =
      let m = sqrt (x*x + y*y)
      in if m < 1e-6 then (0,0) else (x/m, y/m)

-- Example small map ---------------------------------------------------------
exampleWorldMap :: WorldMap
exampleWorldMap =
  let n0 = MapNode (NodeId 0) "Start" (Just (BuiltIn "built-in-levels/world1/1-1.lvl")) (-300, 0) Level Unlocked
      n1 = MapNode (NodeId 1) "1-2"   (Just (BuiltIn "built-in-levels/world1/1-2.lvl")) (-100, 80) Level Locked
      n2 = MapNode (NodeId 2) "Hub"   Nothing                                  (100,  40) Hub   Locked
      n3 = MapNode (NodeId 3) "Boss"  (Just (BuiltIn "built-in-levels/world1/boss.lvl")) (280,  20) Boss  Locked

      e0 = Edge (EdgeId 0) (NodeId 0) (NodeId 1) Both Straight True
      -- Curved link between 1 and hub
      e1 = Edge (EdgeId 1) (NodeId 1) (NodeId 2) Both (Bezier (-100,80) (-40,140) (40,100) (100,40)) False
      -- Polyline shortcut
      e2 = Edge (EdgeId 2) (NodeId 0) (NodeId 2) Both (Polyline [(-300,0),(-200,-60),(-20,-40),(100,40)]) False
      -- Final straight to boss
      e3 = Edge (EdgeId 3) (NodeId 2) (NodeId 3) AtoB Straight False
  in WorldMap { nodes = [n0,n1,n2,n3], edges = [e0,e1,e2,e3] }
