module View.WorldMap where

import Graphics.Gloss
import Data.Maybe

import Model.TypesState
import Model.WorldMap
import View.Helpers

-- Main entrypoint for rendering the map. Renders not just the map, but also the player moving around it.
viewWorldMap :: MapState -> Picture
viewWorldMap MapState { wmWorldMap = wm, wmCursor, wmAlong, wmMenuState } =
  let
      playerPic = 
        let 
          -- (Try to) get player's sprite. Not animated, just uses 1st frame in the animation
          playerSprite = case menuPlayerAnim wmMenuState of
            (anim:_) -> case anim of
              (frame:_) -> withScale (0.6, 0.6) frame
              _         -> blank
            _ -> blank
          -- Where the player is on the map
          (px, py) = case wmAlong of
            Just (_, pts, _, t) -> polylineAt pts t
            Nothing -> fromMaybe (0, 0) (nodePos wm wmCursor)
        in translate px py playerSprite
      nodesPic = pictures [ renderNode n | n <- nodes wm]
      edgesPic = pictures [ renderEdge (edgeToPolyline wm e) | e <- edges wm]

  in Pictures [ edgesPic, nodesPic, playerPic ]

-- Render nodes as filled circles with simple color coding
renderNode :: MapNode -> Picture
renderNode MapNode{pos=(x,y), nodeState, nodeType} =
  let stateCol = case nodeState of
        Locked    -> makeColorI 0 0 0 255                     -- black
        Completed -> makeColorI 50 80 220 255                 -- blue
        Unlocked  -> case nodeType of
                        Hub  -> makeColorI 160 160 160 255    -- grey for hubs
                        _    -> makeColorI 220 50 50 255      -- red for playable levels
      dot   = color stateCol (circleSolid 12)
      outline = color (makeColorI 255 255 255 180) (thickCircle 12 2)
  in translate x y (pictures [dot, outline])

renderEdge :: [Point] -> Picture
renderEdge ps = color (makeColorI 255 255 255 120) (polyline ps)
