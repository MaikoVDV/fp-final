module View.WorldMap where

import Graphics.Gloss

import Model.WorldMap (renderWorldMap, polylineAt, nodePos)
import Model.Types
import View.Helpers

viewWorldMap :: MapState -> Picture
viewWorldMap MapState { wmWorldMap, wmCursor, wmAlong, wmMenuState } =
  let base = renderWorldMap wmWorldMap
      pos = case wmAlong of
        Just (_, pts, _, t) -> polylineAt pts t
        Nothing -> case nodePos wmWorldMap wmCursor of
          Just p  -> p
          Nothing -> (0,0)
      playerPic = case menuPlayerAnim wmMenuState of
        (anim:_) -> case anim of
          (frame:_) -> withScale (0.6, 0.6) frame
          _         -> blank
        _ -> blank
      (px,py) = pos
  in Pictures [ base, translate px py playerPic ]