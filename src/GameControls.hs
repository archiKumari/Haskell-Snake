module GameControls where

import Control.Lens
import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import Brick
import Brick.Types
import Graphics.Vty
import Control.Lens

import Types
import Movement

commandHandler :: GameState -> BrickEvent t () -> EventM n (Next GameState)
commandHandler gs (VtyEvent (EvKey (KChar 'q') [])) = halt gs
commandHandler gs event = case gs ^. gsGameStatusL of
  Initial  -> initialHandler  gs event
  ModeSelect -> modeSelectHandler gs event
  Paused   -> pausedHandler   gs event
  Playing  -> playingHandler  gs event
  GameOver -> gameOverHandler gs event
  
initialHandler :: GameState -> BrickEvent t () -> EventM n (Next GameState)
initialHandler gs (VtyEvent (EvKey (KChar 's') [])) = continue $ gs & (gsGameStatusL .~ Playing)
initialHandler gs (VtyEvent (EvKey (KChar 'm') [])) = continue $ gs & (gsGameStatusL .~ ModeSelect)
initialHandler gs _ = continue gs

modeSelectHandler :: GameState -> BrickEvent t () -> EventM n (Next GameState)
modeSelectHandler gs (VtyEvent (EvKey (KChar '1') [])) = continue normalModeGS
modeSelectHandler gs (VtyEvent (EvKey (KChar '2') [])) = continue infiniteModeGS
modeSelectHandler gs _ = continue gs

playingHandler :: GameState -> BrickEvent t () -> EventM n (Next GameState)
playingHandler gs (VtyEvent (EvKey KUp [])) =    continue $ turn UP gs
playingHandler gs (VtyEvent (EvKey KRight [])) = continue $ turn RIGHT gs 
playingHandler gs (VtyEvent (EvKey KDown [])) =  continue $ turn DOWN gs
playingHandler gs (VtyEvent (EvKey KLeft [])) =  continue $ turn LEFT gs
playingHandler gs (VtyEvent (EvKey (KChar ' ') [])) = continue $ gs & (gsGameStatusL .~ Paused)
playingHandler gs (AppEvent ()) = movementHandler gs
playingHandler gs _ = continue gs

pausedHandler :: GameState -> BrickEvent t () -> EventM n (Next GameState)
pausedHandler gs (VtyEvent (EvKey (KChar ' ') [])) = continue $ gs & (gsGameStatusL .~ Playing)
pausedHandler gs _ = continue gs

gameOverHandler :: GameState -> BrickEvent t () -> EventM n (Next GameState)
gameOverHandler gs (VtyEvent (EvKey KEnter [])) = continue $ initialGS & (gsModeL .~ (gs ^. gsModeL))
gameOverHandler gs (VtyEvent (EvKey (KChar 'm') [])) = continue $ initialGS & (gsGameStatusL .~ ModeSelect)
gameOverHandler gs _ = continue gs

initialGS :: GameState
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
    Playing

normalModeGS :: GameState
normalModeGS = 
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
    Playing

infiniteModeGS :: GameState
infiniteModeGS = 
  GameState 
    (Snake (Cord 1 7) [Cord 1 6,Cord 1 5] UP) 
    (20,20) 
    (Cord 5 6) 
    0 
    3 
    0 
    0 
    1 
    Infinite
    Playing
 