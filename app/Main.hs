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
  { appDraw = pure . drawGameObject 
  , appChooseCursor = neverShowCursor 
  , appHandleEvent = handleGs
  , appStartEvent = return
  , appAttrMap = const theMap
  }

runApp :: IO GameState
runApp = do 
  let builder = mkVty defaultConfig
  initialVty <- builder
  bChan <- newBChan 200
  _ <- forkIO (tick bChan)
  customMain initialVty builder (Just bChan) app initialGS
  where 
   initialGS = GameState (Snake (Cord 1 7) [Cord 1 6,Cord 1 5,Cord 1 4] UP) (20,20) (Cord 5 6) 0 0 0 0 0 0 Normal

tick :: BChan () -> IO ()
tick bChan = do
  threadDelay 100000
  writeBChan bChan ()
  tick bChan

{-runApp' :: IO GameState
runApp' = defaultMain app initialGS
  where 
   initialGS = GameState (Snake (Cord 1 7) [Cord 1 6,Cord 1 5,Cord 1 4] UP) (20,20) (Cord 5 6) 0 0 0 0 0 0-}

