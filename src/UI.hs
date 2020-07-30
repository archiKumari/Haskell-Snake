{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module UI where

import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import Brick
import Brick.Types
import Graphics.Vty
import Lens.Micro   ((^.), (&), (.~), (%~))

import Types

drawGameObject :: GameState -> Widget ()
drawGameObject gs = Widget Fixed Fixed $
  return $ emptyResult &
    imageL .~ pad 38 3 0 0 gameImg
      where gameImg = vertCat $ fmap (mkRowImg gs) rowMap
            rowMap = mkRowMap numRow numCol
            numRow = fst $ gsSize gs 
            numCol = snd $ gsSize gs

mkRowImg :: GameState -> [Cordinate] -> Image
mkRowImg gs cords = horizCat $ fmap (mkCellImg gs) cords

mkCellImg :: GameState -> Cordinate -> Image
mkCellImg gs cord 
  | cord == gsFoodPos gs = charFill bgColor1 '🍅' 1 1
  | cord == sHead (gsSnake gs) = charFill  bgColor1 '@' 2 1
  | cord `elem` (sTail $ gsSnake gs) = charFill bgColor1 '@' 2 1
  | odd (xCord cord) && odd (yCord cord) = charFill bgColor1 ' ' 2 1
  | even (xCord cord) && even (yCord cord) = charFill bgColor1 ' ' 2 1
  | otherwise = charFill bgColor1 ' ' 2 1
  where 
    bgColor1 = V.black `on` V.rgbColor 50 168 160
    bgColor2 = V.black `on` V.rgbColor 50 135 168
    

mkRowMap :: Int -> Int -> [[Cordinate]]
mkRowMap 0 _ = []
mkRowMap _ 0 = []
mkRowMap row col = [Cord m n | m <- [row], n <- [1..col]] : mkRowMap (row-1) col 

-- | Map of Attributes to be used when rendering
theMap::A.AttrMap
theMap = A.attrMap V.defAttr 
     [("edit", V.black `on` bColor)
     ,("status", flip V.withStyle V.bold $ V.white `on` V.black)
     ,("command",V.white `on` V.blue)
     ,("focussed",V.black `on` bColor)]
  where
    bColor = V.rgbColor 240 240 240

