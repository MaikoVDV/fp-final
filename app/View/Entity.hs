module View.Entity where

import Graphics.Gloss
import qualified Data.Map as Map
import Data.Ord

import Model.Types
import qualified Model.Types as Types
import Model.Config
import View.Helpers
  
getEntityAnim :: AnimMap -> EntityType -> Animation
getEntityAnim m t = Map.findWithDefault [blank] t m

renderEntity :: Float -> AnimMap -> Int -> Entity -> Picture
renderEntity tPx m fCtx (EGoomba   _ Goomba { goombaPos, goombaDir, goombaMode })   =
  let anim = getEntityAnim m TGoomba
  in renderGoomba tPx anim fCtx goombaPos goombaDir goombaMode
renderEntity tPx m fCtx (EKoopa    _ Koopa { koopaPos, koopaDir })      = renderEntity' tPx (getEntityAnim m TKoopa)   fCtx koopaPos   koopaDir
renderEntity tPx m fCtx (EPowerup  _ Powerup{ powerupPos, powerupDir }) = renderEntity' tPx (getEntityAnim m TPowerup) fCtx powerupPos powerupDir
renderEntity tPx m fCtx (ECoin     _ Coin  { coinPos })                 = renderEntity' tPx (getEntityAnim m TCoin)    fCtx coinPos    Types.Left
renderEntity _   _ _    (EPlatform _)                                   = blank

renderEntity' :: Float -> Animation -> Int -> Point -> MoveDir -> Picture
renderEntity' tPx anim fCtx (x, y) dir = 
  let  
    spriteIdx = (fCtx `div` frameTime) `mod` length anim
    sprite = anim !! spriteIdx
    spriteScaled = withTileScale tPx sprite
    spriteFlipped = scale (dirToPictureScaleX dir) 1 spriteScaled
  in translate (x * tPx) (y * tPx) spriteFlipped 

-- Goomba: walk cycles first N-1 frames; shelled shows last frame
renderGoomba :: Float -> Animation -> Int -> Point -> MoveDir -> GoombaMode -> Picture
renderGoomba tPx anim fCtx (x, y) dir mode =
  let n = length anim
      idx = case mode of
        GShelled _ -> max 0 (n - 1)
        GWalking   -> let k = max 1 (n - 1)
                      in (fCtx `div` frameTime) `mod` k
      sprite = anim !! (min (n - 1) idx)
      spriteScaled = withTileScale tPx sprite
      spriteFlipped = scale (dirToPictureScaleX dir) 1 spriteScaled
  in translate (x * tPx) (y * tPx) spriteFlipped


renderPlayer :: Float -> Player -> Int -> Picture
renderPlayer tilePixels p@Player { playerPos = (x, y), health, playerAnim = anims, lastMoveDir, invulnTimeLeft } fCtx = 
  let 
    moveDir = if lastMoveDir == -1 then 1 else -1
    anim = anims !! clamp (0, length anims - 1) (health - 1)
    movingInput = moveLeftHeld p || moveRightHeld p
    clock = playerAnimClock p
    spriteIdx = if movingInput then (floor clock `mod` length anim) else 0
    sprite = anim !! spriteIdx
    spriteScaled = withTileScale tilePixels sprite
    spriteFlipped = scale moveDir 1 spriteScaled
    -- Flicker during invulnerability: toggle alpha 0.5/1.0 every flickerInterval, starting at 0.5
    alphaVal =
      if invulnTimeLeft <= 0 then 1.0 else
        let elapsed = invulnDuration - invulnTimeLeft
            k = floor (elapsed / flickerInterval) :: Int
        in if even k then 0.5 else 1.0
  in translate (x * tilePixels) (y * tilePixels) (color (makeColor 1 1 1 alphaVal) spriteFlipped)