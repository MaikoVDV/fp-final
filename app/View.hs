module View where

import Graphics.Gloss
import qualified Data.Map as Map

import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gs = 
  Pictures 
  [ renderWorld gs
  , renderEntities gs
  ]

renderWorld :: GameState -> Picture
renderWorld GameState { world, tileMap, tileSize } = 
  Pictures [
    translate (fromIntegral (x * tileSize)) (fromIntegral ((-y) * tileSize)) (getTileSprite tileMap tile)
    | (y, row)  <- zip [0..] $ grid world
    , (x, tile) <- zip [0..] row
  ]

renderEntities :: GameState -> Picture
renderEntities GameState { entities } = Pictures $ map renderEntity entities

renderEntity :: Entity -> Picture
renderEntity (EPlayer p)        = renderPlayer p
renderEntity (EGoomba g)        = renderGoomba g
renderEntity (EKoopa  k)        = renderKoopa  k
renderEntity EPowerup           = blank
renderEntity EMovingPlatform    = blank

getTileSprite :: TileMap -> Tile -> Picture
getTileSprite m t = Map.findWithDefault blank t m

-- Deze kunnen we misschien allemaal in een RenderEntity functie stoppen
-- door bijv. alle entities een Renderable typeclass te geven waarin
-- een functie staat zoals getSprite of getAnimation of zoiets
renderPlayer :: Player -> Picture
renderPlayer Player { playerPos = (x, y), playerSprite } = 
  translate x y $ head playerSprite -- dit klopt nog niet helemaal met animaties enz

renderGoomba :: Goomba -> Picture
renderGoomba = undefined

renderKoopa :: Koopa -> Picture
renderKoopa = undefined