module Movement where

import Types

import Lens.Micro   ((^.), (&), (.~), (%~))
import Test.QuickCheck
import Brick.Types
import Brick
import Control.Monad.IO.Class

gameHandler :: GameState -> EventM n (Next GameState)
gameHandler gs = case gs ^. gsSnakeL ^. sHeadL of
  pos | pos == gs ^. gsFoodPosL -> do
    let snHead   = gs ^. gsSnakeL ^. sHeadL
        snTail   = gs ^. gsSnakeL ^. sTailL
        snDir = gs ^. gsSnakeL ^. sDirL
        foodPos  = gs ^. gsFoodPosL
        gridSize = gs ^. gsSizeL
        newSnake = Snake foodPos (snHead : snTail) snDir
    newFoodPos <- liftIO $ genFoodPos gridSize
    continue $ gs & (gsSnakeL .~ newSnake).(gsFoodPosL .~ newFoodPos)
  anythingElse -> do  
    let snHead = gs ^. gsSnakeL ^. sHeadL
        snTail  = gs ^. gsSnakeL ^. sTailL
        snDir = gs ^. gsSnakeL ^. sDirL
        newSHead = getNextCord snHead snDir
        newSnake = Snake newSHead (snHead:(init snTail)) snDir
    continue $ gs & gsSnakeL .~ newSnake 
    
getNextCord :: Cordinate -> DIRECTION -> Cordinate
getNextCord (Cord x y) dir = case dir of
    UP    -> Cord (x+1) y
    RIGHT -> Cord x (y+1)
    DOWN  -> Cord (x-1) y
    LEFT  -> Cord x (y-1)

turn :: DIRECTION -> GameState -> GameState 
turn dir gs = gs & gsSnakeL %~ setSnakeDir dir 

setSnakeDir :: DIRECTION -> Snake -> Snake
setSnakeDir d s= s & sDirL .~ d

genFoodPos :: (Int,Int) -> IO Cordinate
genFoodPos (row,col) = do
  x <- generate $ choose (0,row-1)
  y <- generate $ choose (0,col-1)
  return $ Cord y x
