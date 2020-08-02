{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module UI where

import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import Brick
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty
import Lens.Micro   ((^.), (&), (.~), (%~))

import Types


renderGame :: GameState -> Widget ()
renderGame gs = case gs ^. gsGameStatusL of
  GameOver -> drawGameOver gs
  Playing  -> drawGameObject gs
  -- Initial  -> drawInitialWidget

drawGameOver :: GameState ->  Widget ()
drawGameOver gs = center $ Widget Fixed Fixed $ do
  return $ emptyResult &
    imageL .~ gameOverImg
      where
        gameOverImg = vertCat $
          (horizCat . fmap makeColorCellImg ) <$> gameOverStr 

drawGameObject :: GameState -> Widget ()
drawGameObject gs = center $ Widget Fixed Fixed $ do
  return $ emptyResult &
    imageL .~ gameImg 
      where gameImg = vertCat [mkScoreImg gs,mkGameImg gs] 
        
mkGameImg :: GameState -> Image
mkGameImg gs = gameImg
      where gameImg = mkHorizBorder.vertCat $ fmap (mkRowImg gs) rowMap
            rowMap = mkRowMap (gsSize gs)
            bgColor2 = V.black `on` V.rgbColor 50 135 168
            foodPos = show (gs ^. gsFoodPosL)

mkHorizBorder :: Image -> Image
mkHorizBorder img = vertCat [border,img,border]
  where border = charFill brColor ' ' width 1
        width = imageWidth img
        brColor = V.black `on` rgbColor 13 168 10

mkRowImg :: GameState -> [Cordinate] -> Image
mkRowImg gs cords = horizCat [border,rowImg,border]
  where rowImg = horizCat $ fmap (mkCellImg gs) cords
        border = charFill brColor ' ' 2 1
        brColor = V.black `on` rgbColor 13 168 10

mkCellImg :: GameState -> Cordinate -> Image
mkCellImg gs cord
  | cord == gsFoodPos gs = horizCat [charFill bgColor2 '🍉' 1 1]
  | cord == sHead (gsSnake gs) = horizCat [charFill bgColor2 '@' 1 1, charFill bgColor2 ' ' 1 1]
  | elem cord $ (sTail $ gsSnake gs) = horizCat [charFill bgColor2 '@' 1 1,charFill bgColor2 ' ' 1 1]
  | odd (xCord cord) && odd (yCord cord) = charFill bgColor1 ' ' 2 1
  | even (xCord cord) && even (yCord cord) = charFill bgColor1 ' ' 2 1
  | otherwise = charFill bgColor2 ' ' 2 1
  where 
    bgColor1 = V.black `on` V.rgbColor 92 230 90
    bgColor2 = V.black `on` V.rgbColor 55 209 52 

mkRowMap :: GameSize -> [[Cordinate]]
mkRowMap (row,col) | row < 0 || col < 0 = []
mkRowMap (row,col) = [Cord row n | n <- [0..col]] : mkRowMap (row-1,col) 

mkScoreImg :: GameState -> Image
mkScoreImg gs = scoreImg
  where scoreImg  = vertCat [imgLine1,imgLine2]
        imgLine1  = horizCat [fCountImg,charFill brColor ' ' spCount1 1,lCountImg]
        imgLine2  = horizCat [sCountImg,charFill brColor ' ' spCount2 1 ,levelImg]
        fCountImg = horizCat [charFill brColor ' ' 2 1,charFill brColor '🍉' 1 1,string brColor (show foodCount)]
        lCountImg = horizCat [charFill lColor '♥' lifeCount 1,charFill brColor ' ' 2 1] 
        sCountImg = horizCat [charFill brColor ' ' 2 1,charFill brColor '🌟' 1 1,string brColor (show score)]
        levelImg  = horizCat [string brColor (" Level : "++ show level),charFill brColor ' ' 2 1]
        foodCount = gs ^. gsFoodCountL
        lifeCount = gs ^. gsLifeCountL
        score     = gs ^. gsScoreL
        level     = gs ^. gsLevelL
        spCount1  = if foodCount < 10 then 36 else 35
        spCount2  = case score of
          s | s == 0  -> 29
          s | s < 100 -> 28
          _           -> 27 
        brColor = V.black `on` rgbColor 13 168 10
        lColor = flip V.withStyle V.bold $ V.red `on` rgbColor 13 168 10
        
-- | Map of Attributes to be used when rendering
theMap::A.AttrMap
theMap = A.attrMap V.defAttr 
     [("edit", V.black `on` bColor)
     ,("status", flip V.withStyle V.bold $ V.white `on` V.black)
     ,("command",V.white `on` V.blue)
     ,("focussed",V.black `on` bColor)]
  where
    bColor = V.rgbColor 240 240 240


makeColorCellImg :: Char -> Image
makeColorCellImg c | c == ' ' = charFill bgColorLight ' ' 2 1
                   | otherwise = charFill bgColorDark ' ' 2 1
  where 
    bgColorDark = V.black `on` blue
    bgColorLight = V.black `on` V.rgbColor 55 209 52 

gameOverStr =
  ["                            "
  ,"  mmmm                      "
  ," m       mmm   mmmmm   mmm  "
  ," #  mmm #   #  # # #  #   # "
  ," #  m # m   #  # # #  #     "
  ,"  #mmm   mmm#  # # #   #mm  "
  ,"                            "
  ,"                            "
  ,"                            "
  ,"  mmmm                      "
  ," m    m m   m   mmm    mmmm "
  ," #    #  m m   #   #   #    "
  ," #    #  # #   #       #    "
  ,"  #mm#    #     #mm    #    "
  ,"                            "
  ,"                            "
  ]

initGameStr =
   ["#      Press Enter To Play       # "]
