{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Brick

import Types
import UI
import GameControls

main :: IO ()
main = do
  runApp
  return ()

app :: App GameState () ()
app = App
  { appDraw = pure . drawGameObject 
  , appChooseCursor = neverShowCursor 
  , appHandleEvent = handleGs
  , appStartEvent = return
  , appAttrMap = const theMap
  }

runApp :: IO GameState
runApp = defaultMain app initialGS
 where 
   initialGS = GameState (Snake (Cord 1 7) [Cord 1 6,Cord 1 5,Cord 1 4] UP) (20,20) (Cord 5 6) 0 0 0 0 0 0

