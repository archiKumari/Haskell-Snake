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

handleGs :: GameState -> BrickEvent t t1 -> EventM n (Next GameState)
handleGs gs (VtyEvent (EvKey (KChar 'q') [])) = halt gs
handleGs gs (VtyEvent (EvKey (KChar 'm') [])) = gameHandler gs
handleGs gs (VtyEvent (EvKey KUp [])) =    continue $ turn UP gs
handleGs gs (VtyEvent (EvKey KRight [])) = continue $ turn RIGHT gs 
handleGs gs (VtyEvent (EvKey KDown [])) =  continue $ turn DOWN gs
handleGs gs (VtyEvent (EvKey KLeft [])) =  continue $ turn LEFT gs
handleGs gs _ = continue gs
