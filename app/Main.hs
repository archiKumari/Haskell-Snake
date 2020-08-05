{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Brick
import Brick.BChan
import Control.Concurrent
import Graphics.Vty

import Types
import UI
import GameControls

main :: IO ()
main = do
  runApp
  return ()

app :: App GameState () ()
app = App
  { appDraw         = pure . renderGame 
  , appChooseCursor = neverShowCursor 
  , appHandleEvent  = commandHandler
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

runApp :: IO GameState
runApp = do 
  let builder = mkVty defaultConfig
  initialVty  <- builder
  bChan       <- newBChan 200
  _           <- forkIO (tick bChan)
  customMain initialVty builder (Just bChan) app initialGS
  where 
   initialGS =
     GameState 
       (Snake (Cord 1 7) [Cord 1 6,Cord 1 5] UP) 
       (20,20) 
       (Cord 5 6) 
       0 
       3 
       0 
       0 
       1 
       Normal
       Initial --Playing

tick :: BChan () -> IO ()
tick bChan = do
  threadDelay 100000
  writeBChan bChan ()
  tick bChan
