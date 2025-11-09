module View.Main where

import Graphics.Gloss

import Model.TypesState
import View.Game
import View.Menu
import View.WorldMap
import View.Builder


view :: AppState -> IO Picture
view = return . viewPure

viewPure :: AppState -> Picture
viewPure (Menu menuState) = renderMenu menuState
viewPure (Playing gs) = viewGame gs
viewPure (Building bs) = viewBuilder bs
viewPure (WorldMapScreen ms) = viewWorldMap ms