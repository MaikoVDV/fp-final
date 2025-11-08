module View.Main where

import Graphics.Gloss
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Ord

import Model.Types
import qualified Model.Types as Types
import Model.Collider
import Model.InitialState

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