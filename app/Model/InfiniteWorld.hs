module Model.InfiniteWorld
  ( segmentsAheadDefault
  , ensureInfiniteSegments
  ) where

import Control.Exception (SomeException, try)
import Data.List (foldl')
import Model.Collider (generateCollidersForWorld)
import Model.Entity (setId)
import Model.InfiniteSegments (SegmentMeta (..))
import Model.Types
import Model.TypesState
import LevelCodec (loadSegmentWorld)
import System.Random (randomRIO)

segmentsAheadDefault :: Int
segmentsAheadDefault = 2

maxSegmentHeightDelta :: Int
maxSegmentHeightDelta = 4

ensureInfiniteSegments :: GameState -> IO GameState
ensureInfiniteSegments gs@GameState { infiniteState = Nothing } = return gs
ensureInfiniteSegments gs@GameState { infiniteState = Just infState } =
  if null (infSegments infState)
    then return gs
    else do
      (gs', inf') <- fillAhead gs infState
      return gs' { infiniteState = Just inf' }
  where
    fillAhead g inf = do
      let px = fst (playerPos (player g))
          segIdx = currentSegmentIndex px (infSegments inf)
          ahead = length (infSegments inf) - segIdx - 1
      if ahead >= infSegmentsAhead inf
        then return (g, inf)
        else do
          appended <- appendNextSegment g inf
          case appended of
            Nothing        -> return (g, inf)
            Just (g', inf') -> fillAhead g' inf'

appendNextSegment :: GameState -> InfiniteRunState -> IO (Maybe (GameState, InfiniteRunState))
appendNextSegment gs infState =
  case infSegments infState of
    [] -> return Nothing
    segs -> do
      let lastSeg = last segs
          targetHeight = endHeight (activeMeta lastSeg)
      mNextMeta <- pickSegment targetHeight (infSegmentPool infState)
      case mNextMeta of
        Nothing -> return Nothing
        Just meta -> do
          eSegment <- (try (loadSegmentWorld (levelPath meta)) :: IO (Either SomeException (World, [Entity])))
          case eSegment of
            Prelude.Left err -> do
              putStrLn ("Failed to load infinite segment \"" ++ segmentName meta ++ "\": " ++ show err)
              return Nothing
            Prelude.Right (segWorld, segEntities) -> do
              let startX = activeStartX lastSeg + activeWidth lastSeg
                  mergedWorld = mergeWorlds (world gs) segWorld
                  offset = fromIntegral startX
                  shifted = map (shiftEntity offset) segEntities
                  (withIds, nextId) = assignEntityIds (entityIdCounter gs) shifted
                  segWidth = worldWidth segWorld
                  newSeg = ActiveSegment meta startX segWidth
                  gs' =
                    gs
                      { world = mergedWorld
                      , entities = entities gs ++ withIds
                      , entityIdCounter = nextId
                      }
                  inf' = infState { infSegments = segs ++ [newSeg] }
              return (Just (gs', inf'))

pickSegment :: Int -> [SegmentMeta] -> IO (Maybe SegmentMeta)
pickSegment _ [] = return Nothing
pickSegment target metas = do
  let withinDelta m =
        let delta = startHeight m - target
        in delta <= maxSegmentHeightDelta
      pool = filter withinDelta metas
  if null pool
    then do
      putStrLn ("No segment within +" ++ show maxSegmentHeightDelta ++ " blocks above height " ++ show target ++ ".")
      return Nothing
    else do
      idx <- randomRIO (0, length pool - 1)
      return (Just (pool !! idx))

currentSegmentIndex :: Float -> [ActiveSegment] -> Int
currentSegmentIndex px = go 0
  where
    go idx [] = max 0 (idx - 1)
    go idx (seg:rest)
      | px < fromIntegral (segmentEndX seg) = idx
      | otherwise = go (idx + 1) rest

segmentEndX :: ActiveSegment -> Int
segmentEndX seg = activeStartX seg + activeWidth seg

mergeWorlds :: World -> World -> World
mergeWorlds base addition =
  let rowsA = grid base
      rowsB = grid addition
      height = max (length rowsA) (length rowsB)
      widthA = worldWidth base
      widthB = worldWidth addition
      padRows rows width =
        let missing = max 0 (height - length rows)
            airRow = replicate width Air
        in replicate missing airRow ++ rows
      rowsA' = padRows rowsA widthA
      rowsB' = padRows rowsB widthB
      combined = zipWith (++) rowsA' rowsB'
      colliders = generateCollidersForWorld combined
  in base { grid = combined, colliders = colliders, slopes = [] }

worldWidth :: World -> Int
worldWidth w = case grid w of
  []     -> 0
  (r:_)  -> length r

shiftEntity :: Float -> Entity -> Entity
shiftEntity dx (EGoomba gid g@Goomba { goombaPos = (x, y) }) =
  EGoomba gid (g { goombaPos = (x + dx, y) })
shiftEntity dx (EKoopa gid k@Koopa { koopaPos = (x, y) }) =
  EKoopa gid (k { koopaPos = (x + dx, y) })
shiftEntity dx (EPowerup pid p@Powerup { powerupPos = (x, y) }) =
  EPowerup pid (p { powerupPos = (x + dx, y) })
shiftEntity dx (ECoin cid c@Coin { coinPos = (x, y) }) =
  ECoin cid (c { coinPos = (x + dx, y) })
shiftEntity _ entity = entity

assignEntityIds :: Int -> [Entity] -> ([Entity], Int)
assignEntityIds startId ents =
  let (revList, nextId) = foldl' step ([], startId) ents
      step (acc, n) e =
        let e' = setId e n
        in (e' : acc, n + 1)
  in (reverse revList, nextId)
