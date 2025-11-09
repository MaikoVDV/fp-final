module Controller.WorldMap where

import Model.TypesState
import Model.WorldMap

updateMap :: Float -> MapState -> MapState
updateMap _ ms@MapState { wmAlong = Nothing } = ms
updateMap dt ms@MapState { wmAlong = Just (eid, pts, dest, t), wmSpeed } =
  let len = polylineLength pts
  in if len <= 1e-3
        then ms { wmCursor = dest, wmAlong = Nothing }
        else
          let step = wmSpeed * dt / len
              t' = t + step
          in if t' >= 1
                then ms { wmCursor = dest, wmAlong = Nothing }
                else ms { wmAlong = Just (eid, pts, dest, t') }

-- Unlock neighbors and mark current node completed
unlockAfterFinish :: MapState -> MapState
unlockAfterFinish ms@MapState { wmWorldMap = wm, wmCursor = cur } =
  let edges1 = [ if allowsFrom e cur then e { unlocked = True } else e | e <- edges wm ]
      neighborIds = [ other e cur | e <- edges wm, allowsFrom e cur ]
      -- Hubs among immediate neighbors
      isHubId nid = case filter ((== nid) . nodeId) (nodes wm) of
                      (n:_) -> case nodeType n of { Hub -> True; _ -> False }
                      _     -> False
      hubIds = [ nid | nid <- neighborIds, isHubId nid ]
      -- Unlock all edges emanating from unlocked hubs as well
      unlockFromHub e = any (allowsFrom e) hubIds
      edges2 = [ if unlockFromHub e then e { unlocked = True } else e | e <- edges1 ]
      -- Also unlock nodes reachable from those hubs
      neighborIdsFromHubs = [ other e h | e <- edges wm, h <- hubIds, allowsFrom e h ]
      nodes' = [ updateNode n | n <- nodes wm ]
      updateNode n
        | nodeId n == cur = n { nodeState = Completed }
        | nodeId n `elem` (neighborIds ++ neighborIdsFromHubs) = case nodeState n of
            Locked -> n { nodeState = Unlocked }
            _      -> n
        | otherwise = n
      wm' = wm { nodes = nodes', edges = edges2 }
  in ms { wmWorldMap = wm' }
  where
    allowsFrom e nid = case dir e of
      Undirected -> a e == nid || b e == nid
      Both       -> a e == nid || b e == nid
      AtoB       -> a e == nid
      BtoA       -> b e == nid
    other e nid = if a e == nid then b e else a e