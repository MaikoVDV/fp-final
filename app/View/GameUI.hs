module View.GameUI where

import Graphics.Gloss
import qualified Data.Map as Map
import Debug.Trace

import Model.Types
import Model.TypesState
import Model.Config
import View.Entity
import Model.Config (assetTilePixelSize)

-- Renders the in-game hud, which displays hearts, coins collected and remaining lives.
-- Note on scaling: Gloss doesn't really expose window dimension, only screen dimensions. So, our game is in fullscreen so we can
-- just use those values instead of manually tracking window dimensions.
-- Also, the UI elements (hearts, numbers, coins) are all from the same tileset, and so use the same tile size. (so we use the assetTilePixelSize constant)
renderHUD :: Float -> Float -> GameState -> Picture
renderHUD screenW screenH GameState { player, uiHeartFull, uiHeartHalf, uiHeartEmpty, uiHeartGolden, uiCounters, playerLives, coinsCollected, animMap = hudAnimMap } =
  let
    margin = 20 :: Float
    topY = screenH / 2 - margin

    -- Hearts row (left side)
    leftX = -screenW / 2 + margin + (assetTilePixelSize / 2 * 1.5)
    heartScale = (screenH * 0.06) / assetTilePixelSize
    heartSpacing = heartScale * assetTilePixelSize * 1.1
    heartPicFor idx =
      let hp = health player
          fulls = hp `div` 2
          half  = hp `mod` 2
      in if idx < fulls then uiHeartFull else if idx == fulls && half == 1 then uiHeartHalf else uiHeartEmpty
    hearts = [ translate (leftX + fromIntegral i * heartSpacing) (topY - 16)
                  (scale heartScale heartScale (heartPicFor i))
             | i <- [0..2] ]

    -- Lives counter 
    counterScale = (screenH * 0.06) / assetTilePixelSize
    counterSize  = assetTilePixelSize * counterScale
    counterSpacing = counterSize * 0.8
    counterDigits = show playerLives
    counterPics = [ Map.findWithDefault blank ch uiCounters | ch <- counterDigits ]
    goldHeartPic = scale counterScale counterScale uiHeartGolden
    livesWidth = counterSize + counterSpacing * fromIntegral (length counterPics)

    -- Coin counter (right side)
    coinDigits =
      let s = show coinsCollected
      in replicate (max 0 (3 - length s)) '0' ++ s
    coinDigitPics = [ Map.findWithDefault blank ch uiCounters | ch <- coinDigits ]
    coinIconBase = case getEntityAnim hudAnimMap TCoin of
                     (frame:_) -> frame
                     _         -> blank
    coinIconPic = scale counterScale counterScale coinIconBase
    coinWidth = counterSpacing * (1 + fromIntegral (length coinDigitPics))
    gapBetweenCounters = 200
    rightX = screenW / 2 - margin
    totalWidth = coinWidth + gapBetweenCounters + livesWidth
    startX = rightX - totalWidth

    coinBlock =
      translate startX (topY - 16) $
        Pictures $
          coinIconPic :
          [ translate (counterSpacing * (fromIntegral i + 1)) 0 (scale counterScale counterScale p)
          | (i, p) <- zip [0..] coinDigitPics ]

    livesBlock =
      translate (startX + coinWidth + gapBetweenCounters) (topY - 16) $
        Pictures $
          goldHeartPic :
          [ translate (counterSpacing * (fromIntegral i + 1)) 0 (scale counterScale counterScale p)
          | (i, p) <- zip [0..] counterPics ]

  in Pictures (hearts ++ [coinBlock, livesBlock]) 


-- Pause overlay with Resume and Main Menu buttons
renderPauseMenu :: (Int, Int) -> Picture
renderPauseMenu (screenW, screenH) =
  let sw = fromIntegral screenW :: Float
      sh = fromIntegral screenH :: Float
      bg = color (makeColor 0 0 0 0.5) $ polygon [(-sw/2,-sh/2),(sw/2,-sh/2),(sw/2,sh/2),(-sw/2,sh/2)]
      panelW = sw * 0.6
      panelH = sh * 0.3
      panelRect = color (makeColor 1 1 1 0.9) $ polygon [(-panelW/2,-panelH/2),(panelW/2,-panelH/2),(panelW/2,panelH/2),(-panelW/2,panelH/2)]
      panelBorder = color black $ lineLoop [(-panelW/2,-panelH/2),(panelW/2,-panelH/2),(panelW/2,panelH/2),(-panelW/2,panelH/2)]
      titleScale = (panelH * 0.16) / 100
      title = translate (-panelW*0.12) (panelH*0.12) $ color black $ scale titleScale titleScale $ text "Paused"
      btnW = 260; btnH = 90
      btnY = - panelH * 0.15
      resumeX = - panelW * 0.2
      menuX   =   panelW * 0.2
      button label cx cy =
        let rect = polygon [(-btnW/2,-btnH/2),(btnW/2,-btnH/2),(btnW/2,btnH/2),(-btnW/2,btnH/2)]
            border = lineLoop [(-btnW/2,-btnH/2),(btnW/2,-btnH/2),(btnW/2,btnH/2),(-btnW/2,btnH/2)]
            s = (btnH * 0.45) / 100
            lbl = translate (-(fromIntegral (length label) * 7) * s) (-12) $ scale s s $ text label
        in translate cx cy $ Pictures [ color (makeColor 1 1 1 0.85) rect, color black border, color black lbl ]
  in Pictures [ bg
              , panelRect, panelBorder, title
              , button "Resume"   resumeX btnY
              , button "Main Menu" menuX   btnY
              ]