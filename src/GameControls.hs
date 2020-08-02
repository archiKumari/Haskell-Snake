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

handleGs :: GameState -> BrickEvent t () -> EventM n (Next GameState)
handleGs gs (VtyEvent (EvKey (KChar 'q') [])) = halt gs
handleGs gs (VtyEvent (EvKey KUp [])) =    continue $ turn UP gs
handleGs gs (VtyEvent (EvKey KRight [])) = continue $ turn RIGHT gs 
handleGs gs (VtyEvent (EvKey KDown [])) =  continue $ turn DOWN gs
handleGs gs (VtyEvent (EvKey KLeft [])) =  continue $ turn LEFT gs
handleGs gs (VtyEvent (EvKey kEnter [])) = continue $ GameState (Snake (Cord 1 7) [Cord 1 6,Cord 1 5,Cord 1 4] UP) (20,20) (Cord 5 6) 0 3 0 0 1 Normal Playing
handleGs gs (AppEvent ()) = gameHandler gs
handleGs gs _ = continue gs
  where 
    initialGS =
      GameState 
       (Snake (Cord 1 7) [Cord 1 6,Cord 1 5,Cord 1 4] UP) 
       (20,20) 
       (Cord 5 6) 
       0 
       3 
       0 
       0 
       1 
       Normal
       Playing
