module Movement where

import Types

import Lens.Micro   ((^.), (&), (.~), (%~))
import Test.QuickCheck
import Brick.Types
import Brick
import Control.Monad.IO.Class

gameHandler :: GameState -> EventM n (Next GameState)
gameHandler gs = case gs ^. gsSnakeL ^. sHeadL of
  pos | pos == gs ^. gsFoodPosL -> foodPosHandler gs
  pos | pos `elem` drop 1 (gs ^. gsSnakeL ^. sTailL) -> movementHandler gs 
  (Cord x y) | x == fst (gs ^. gsSizeL) || x < 0 -> movementHandler gs
  (Cord x y) | y == snd (gs ^. gsSizeL) || y < 0 -> movementHandler gs 
  anythingElse -> do  
    let snHead = gs ^. gsSnakeL ^. sHeadL
        snTail  = gs ^. gsSnakeL ^. sTailL
        snDir = gs ^. gsSnakeL ^. sDirL
        newSHead = getNextCord snHead snDir
        newSnake = Snake newSHead (init $ snHead:snTail) snDir
    continue $ gs & gsSnakeL .~ newSnake 

foodPosHandler :: GameState -> EventM n (Next GameState)
foodPosHandler gs = do
  let snHead   = gs ^. gsSnakeL ^. sHeadL
      snTail   = gs ^. gsSnakeL ^. sTailL
      snDir = gs ^. gsSnakeL ^. sDirL
      foodPos  = gs ^. gsFoodPosL
      gridSize = gs ^. gsSizeL
      newSnake = Snake foodPos (snHead : snTail) snDir
      snakeCords = snHead : snTail
  newFoodPos <- liftIO $ genFoodPos gridSize snakeCords
  continue $ gs & (gsSnakeL .~ newSnake).(gsFoodPosL .~ newFoodPos)

movementHandler :: GameState -> EventM n (Next GameState)
movementHandler = halt

getNextCord :: Cordinate -> DIRECTION -> Cordinate
getNextCord (Cord x y) dir = case dir of
    UP    -> Cord (x+1) y
    RIGHT -> Cord x (y+1)
    DOWN  -> Cord (x-1) y
    LEFT  -> Cord x (y-1)

turn :: DIRECTION -> GameState -> GameState 
turn dir gs = gs & gsSnakeL %~ setSnakeDir dir'
  where
    oldDir = gs ^. gsSnakeL . sDirL
    dir' =
      if isOppositeDir dir oldDir 
        then oldDir
        else dir   

isOppositeDir :: DIRECTION -> DIRECTION -> Bool
isOppositeDir UP    DOWN  = True        
isOppositeDir DOWN  UP    = True        
isOppositeDir LEFT  RIGHT = True        
isOppositeDir RIGHT LEFT  = True        
isOppositeDir _     _     = False        

setSnakeDir :: DIRECTION -> Snake -> Snake
setSnakeDir d s= s & sDirL .~ d

genFoodPos :: GameSize -> [Cordinate] -> IO Cordinate
genFoodPos (row,col) snakeCords = do
  x <- generate $ choose (0,row-1)
  y <- generate $ choose (0,col-1)
  let cord = Cord y x
  if Cord y x `elem` snakeCords then genFoodPos (row,col) snakeCords else return (Cord y x)
