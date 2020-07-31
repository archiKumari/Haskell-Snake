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

drawGameObject :: GameState -> Widget ()
drawGameObject gs = center $ Widget Fixed Fixed $
  return $ emptyResult &
    imageL .~ {-pad 38 3 0 0-} gameImg
      where gameImgWithB = mkImgBorder (gs ^. gsSizeL) gameImg
            gameImg = vertCat $ fmap (mkRowImg gs) rowMap
            rowMap = mkRowMap (gsSize gs)
            bgColor2 = V.black `on` V.rgbColor 50 135 168
            foodPos = show (gs ^. gsFoodPosL)

mkImgBorder :: GameSize -> Image -> Image 
mkImgBorder (row,col) img = horizCat [leftBorder,img,rightBorder]
  where 
    leftBorder = vertCat (replicate col unitBorder)
    rightBorder = vertCat (replicate col unitBorder)
    unitBorder = charFill brColor ' ' 3 1
    brColor = V.black `on` rgbColor 17 214 159

mkRowImg :: GameState -> [Cordinate] -> Image
mkRowImg gs cords = horizCat $ fmap (mkCellImg gs) cords

mkCellImg :: GameState -> Cordinate -> Image
mkCellImg gs cord 
  | cord == gsFoodPos gs = charFill bgColor2 '🍅' 1 1
  | cord == sHead (gsSnake gs) = horizCat [charFill bgColor2 '@' 1 1, charFill bgColor2 ' ' 1 1]
  | cord `elem` (sTail $ gsSnake gs) = horizCat [charFill bgColor2 '@' 1 1,charFill bgColor2 ' ' 1 1]
  | odd (xCord cord) && odd (yCord cord) = charFill bgColor2 ' ' 2 1
  | even (xCord cord) && even (yCord cord) = charFill bgColor2 ' ' 2 1
  | otherwise = charFill bgColor2 ' ' 2 1
  where 
    bgColor1 = V.black `on` V.rgbColor 50 168 160
    bgColor2 = V.black `on` V.rgbColor 50 135 168     

mkRowMap :: GameSize -> [[Cordinate]]
mkRowMap (row,col) | row < 0 || col < 0 = []
mkRowMap (row,col) = [Cord row n | n <- [0..col]] : mkRowMap (row-1,col) 

-- | Map of Attributes to be used when rendering
theMap::A.AttrMap
theMap = A.attrMap V.defAttr 
     [("edit", V.black `on` bColor)
     ,("status", flip V.withStyle V.bold $ V.white `on` V.black)
     ,("command",V.white `on` V.blue)
     ,("focussed",V.black `on` bColor)]
  where
    bColor = V.rgbColor 240 240 240

