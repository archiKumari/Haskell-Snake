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

-- Main function
main :: IO ()
main = do
  runApp
  return ()

-- | App function for specifying different functions for different features of the Game
app :: App GameState () ()
app = App
  { appDraw         = pure . renderGame 
  , appChooseCursor = neverShowCursor 
  , appHandleEvent  = commandHandler
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

-- | Function to initialize main function with initial GameState to run the Snake Game
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
       Initial 

-- | Function to write AppEvent () in the buffer after the specified time
tick :: BChan () -> IO ()
tick bChan = do
  threadDelay 100000
  writeBChan bChan ()
  tick bChan
