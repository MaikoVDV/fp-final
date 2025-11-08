module View.Menu where

import Graphics.Gloss

import Model.Types

renderMenu :: MenuState -> Picture
renderMenu MenuState { menuDebugMode, menuScreenSize, menuFocus, menuPage, menuCustomFiles, menuInput } =
  let (screenW, screenH) = menuScreenSize
      sw = fromIntegral screenW
      sh = fromIntegral screenH
      titleScale = (sh * 0.12) / 100.0

      buttonPic btnW btnH label focused y =
        let bg    = if focused then makeColor 1 1 0 0.35 else makeColor 1 1 1 0.15
            borderCol = if focused then yellow else white
            rect = polygon [(-btnW/2, -btnH/2), (btnW/2, -btnH/2), (btnW/2, btnH/2), (-btnW/2, btnH/2)]
            border = lineLoop [(-btnW/2, -btnH/2), (btnW/2, -btnH/2), (btnW/2, btnH/2), (-btnW/2, btnH/2)]
            textScale = (btnH * 0.45) / 100.0
            labelPic = scale textScale textScale $ text label
            labelShift = - (fromIntegral (length label) * 7) * textScale
        in translate 0 y $ Pictures [ color bg rect, color borderCol border, translate labelShift (-12) (color white labelPic) ]

      debugPic
        | menuDebugMode = translate (-sw/2 + 10) (-sh/2 + 20) $ color yellow $ scale 0.12 0.12 $ text "Debug"
        | otherwise = blank

  in case menuPage of
      MainMenu ->
        let titlePic = translate (-sw * 0.25) (sh * 0.23) $ color white $ scale titleScale titleScale $ text "FP Final"
            btnW = 460 :: Float
            btnH = 90  :: Float
            ys   = [150, 50, -50, -150] :: [Float]
            labels = ["Play", "Builder", "Custom Levels", "Infinite Mode"]
            buttonsPic = Pictures
              [ buttonPic btnW btnH label (menuFocus == ix) y
              | (ix, (label, y)) <- zip [0..] (zip labels ys)
              ]
        in Pictures [titlePic, buttonsPic, debugPic]

      CustomLevels ->
        let titlePic = translate (-sw * 0.33) (sh * 0.23) $ color white $ scale titleScale titleScale $ text "Custom Levels"
            btnW = 600 :: Float
            btnH = 70  :: Float
            yOf i = 120 - fromIntegral i * 80 :: Float
            items = case menuCustomFiles of
              [] -> [translate (-200) 20 $ color white $ scale 0.18 0.18 $ text "No levels found in ./levels"]
              fs -> [ buttonPic btnW btnH f (menuFocus == ix) (yOf ix) | (ix, f) <- zip [0..] fs ]
        in Pictures ([titlePic] ++ items ++ [debugPic])

      BuilderSelect ->
        let titlePic = translate (-sw * 0.33) (sh * 0.23) $ color white $ scale titleScale titleScale $ text "Builder: Select Level"
            btnW = 600 :: Float
            btnH = 70  :: Float
            yOf i = 160 - fromIntegral i * 80 :: Float
            -- Decode focus to (row, col)
            focusRow = menuFocus `div` 2
            focusCol = menuFocus `mod` 2
            headerBtn = buttonPic btnW btnH "New Level" (focusRow == 0) (yOf 0)
            filesPic = case menuCustomFiles of
              [] -> [translate (-250) (yOf 1) $ color white $ scale 0.18 0.18 $ text "No levels found in ./levels"]
              fs ->
                let delW = 160 :: Float
                    delH = btnH
                    delX = btnW/2 + 40 + delW/2  -- right of the file button
                    delLabelScale = (delH * 0.45) / 100.0
                    delRect = polygon [(-delW/2, -delH/2), (delW/2, -delH/2), (delW/2, delH/2), (-delW/2, delH/2)]
                    delBorder = lineLoop [(-delW/2, -delH/2), (delW/2, -delH/2), (delW/2, delH/2), (-delW/2, delH/2)]
                    delPic focused y =
                      let bg = if focused then makeColor 1 0.6 0.6 0.5 else makeColor 1 0 0 0.25
                          br = if focused then makeColor 1 0.3 0.3 1 else makeColor 1 0 0 1
                      in translate delX y $ Pictures [ color bg delRect
                                                      , color br delBorder
                                                      , translate (-30) (-12) $ color white $ scale delLabelScale delLabelScale $ text "Delete"
                                                      ]
                in [ Pictures [ buttonPic btnW btnH f (focusRow == ix+1 && focusCol == 0) (yOf (ix+1))
                              , delPic (focusRow == ix+1 && focusCol == 1) (yOf (ix+1))
                              ]
                   | (ix, f) <- zip [0..] fs
                   ]
        in Pictures ([titlePic, headerBtn] ++ filesPic ++ [debugPic])

      BuilderName ->
        let titlePic = translate (-sw * 0.33) (sh * 0.23) $ color white $ scale titleScale titleScale $ text "Builder: New Level"
            prompt  = translate (-sw * 0.35) 40 $ color white $ scale 0.2 0.2 $ text "Enter level name:"
            inputBxW = 800; inputBxH = 80
            inputRect = color (makeColor 1 1 1 0.15) $ translate 0 (-20) $ polygon [(-inputBxW/2,-inputBxH/2),(inputBxW/2,-inputBxH/2),(inputBxW/2,inputBxH/2),(-inputBxW/2,inputBxH/2)]
            label    = let s = menuInput
                           scaleT = 0.2
                           shiftX = - (fromIntegral (length s) * 7) * scaleT
                       in translate shiftX (-35) $ color white $ scale scaleT scaleT $ text s
            hint     = translate (-sw * 0.28) (-120) $ color white $ scale 0.15 0.15 $ text "Enter to confirm | Esc to cancel"
        in Pictures [titlePic, prompt, inputRect, label, hint, debugPic]
