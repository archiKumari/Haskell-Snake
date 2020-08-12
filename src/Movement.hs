module Movement where

import Types

import Lens.Micro   ((^.), (&), (.~), (%~))
import Test.QuickCheck
import Brick.Types
import Brick
import Control.Monad.IO.Class

-- | Takes GameState and returns next GameState according to the movement of Snake in the Game
movementHandler :: GameState -> EventM n (Next GameState)
movementHandler gs = case gs ^. (gsSnakeL . sHeadL) of
  pos | pos == gs ^. gsFoodPosL                       -> foodPosHandler gs
  pos | elem pos $ drop 1 (gs ^. (gsSnakeL . sTailL))  -> overlapHandler gs 
  (Cord x y) | x == 1 + fst (gs ^. gsSizeL) || x < 0     -> edgeHandler gs
  (Cord x y) | y == 1 + snd (gs ^. gsSizeL) || y < 0     -> edgeHandler gs 
  anythingElse -> do  
    let snHead   = gs ^. (gsSnakeL . sHeadL)
        snTail   = gs ^. (gsSnakeL . sTailL)
        snDir    = gs ^. (gsSnakeL . sDirL)
        newSHead = getNextCord snHead snDir
        newSnake = Snake newSHead (init $ snHead:snTail) snDir
    continue $ gs & gsSnakeL .~ newSnake 

-- | Takes GameState and returns next GameState with new food position
-- when Snake reaches to the Food
foodPosHandler :: GameState -> EventM n (Next GameState)
foodPosHandler gs = do
  let snHead      = gs ^. (gsSnakeL . sHeadL)
      snTail      = gs ^. (gsSnakeL . sTailL)
      snDir       = gs ^. (gsSnakeL . sDirL)
      foodPos     = gs ^. gsFoodPosL
      gridSize    = gs ^. gsSizeL
      newSnake    = Snake foodPos (snHead : snTail) snDir
      snakeCords  = snHead : snTail
  newFoodPos <- liftIO $ genFoodPos gridSize snakeCords
  continue $ gs & (gsSnakeL .~ newSnake).(gsFoodPosL .~ newFoodPos).(gsFoodCountL %~ (+1)).(gsScoreL %~ (+10))

-- | Takes GameState and returns next GameState according to Game Mode 
-- when the Snake overlaps itself
overlapHandler :: GameState -> EventM n (Next GameState)
overlapHandler gs = case gs ^. gsModeL of
  Infinite -> do  
    let snHead   = gs ^. (gsSnakeL . sHeadL)
        snTail   = gs ^. (gsSnakeL . sTailL)
        snDir    = gs ^. (gsSnakeL . sDirL)
        newSHead = getNextCord snHead snDir
        newSnake = Snake newSHead (init $ snHead:snTail) snDir
    continue $ gs & gsSnakeL .~ newSnake
  _ -> do 
    let lifeCount = gs ^. gsLifeCountL
        newSnake = Snake (Cord 1 7) [Cord 1 6,Cord 1 5] UP
    case lifeCount of
      l | l == 1 -> continue (gs & gsGameStatusL .~ GameOver)
      _ -> continue $ gs & (gsSnakeL .~ newSnake).(gsLifeCountL %~ pred)
  
-- | Takes GameState and returns next GameState according to Game Mode
-- when the Snake strikes the edge of game grid
edgeHandler :: GameState -> EventM n (Next GameState)
edgeHandler gs = case gs ^. gsModeL of
  Infinite -> case gs ^. (gsSnakeL . sHeadL) of
    (Cord x y) | x < 0 -> do 
      let edgeCord = snd (gs ^. gsSizeL)
          newSnHead = Cord edgeCord y
          newSnake = gs ^. gsSnakeL & (sHeadL .~ newSnHead)
      continue $ gs & gsSnakeL .~ newSnake
    (Cord x y) | x == 1 + fst (gs ^. gsSizeL) -> do
      let newSnHead = Cord 0 y
          newSnake = gs ^. gsSnakeL & (sHeadL .~ newSnHead)
      continue $ gs & gsSnakeL .~ newSnake
    (Cord x y) | y < 0 -> do 
      let edgeCord = fst (gs ^. gsSizeL)
          newSnHead = Cord x edgeCord
          newSnake = gs ^. gsSnakeL & (sHeadL .~ newSnHead)
      continue $ gs & gsSnakeL .~ newSnake
    (Cord x y) | y == 1 + snd (gs ^. gsSizeL) -> do
      let newSnHead = Cord x 0 
          newSnake = gs ^. gsSnakeL & (sHeadL .~ newSnHead)
      continue $ gs & gsSnakeL .~ newSnake
  _ -> do 
    let lifeCount = gs ^. gsLifeCountL
        newSnake = Snake (Cord 1 7) [Cord 1 6,Cord 1 5] UP
    case lifeCount of
      l | l == 1 -> continue (gs & gsGameStatusL .~ GameOver)
      _ -> continue $ gs & (gsSnakeL .~ newSnake).(gsLifeCountL %~ pred)
  
-- | Takes a Cordinate and current direction of Snake and 
-- returns next cordinate for the snake
getNextCord :: Cordinate -> DIRECTION -> Cordinate
getNextCord (Cord x y) dir = case dir of
    UP    -> Cord (x+1) y
    RIGHT -> Cord x (y+1)
    DOWN  -> Cord (x-1) y
    LEFT  -> Cord x (y-1)

-- | Takes a direction and returns next clockwise direction
getNextDir :: DIRECTION -> DIRECTION
getNextDir UP = RIGHT
getNextDir DOWN = LEFT
getNextDir RIGHT = UP
getNextDir LEFT = DOWN

-- | Takes a direction and current GameState and returns new GameState with changed direction
turn :: DIRECTION -> GameState -> GameState 
turn dir gs = gs & gsSnakeL %~ setSnakeDir dir'
  where
    oldDir = gs ^. gsSnakeL . sDirL
    dir'   =
      if isOppositeDir dir oldDir 
        then oldDir
        else dir   

-- | Takes two directions and checks if both directions are opposite 
isOppositeDir :: DIRECTION -> DIRECTION -> Bool
isOppositeDir UP    DOWN  = True        
isOppositeDir DOWN  UP    = True        
isOppositeDir LEFT  RIGHT = True        
isOppositeDir RIGHT LEFT  = True        
isOppositeDir _     _     = False        

-- | Takes a direction and a snake and returns a new snake with changed direction
setSnakeDir :: DIRECTION -> Snake -> Snake
setSnakeDir d s = s & sDirL .~ d

-- | Takes GameSize and cordinates of Snake and returns a cordinate
-- for new food position  
genFoodPos :: GameSize -> [Cordinate] -> IO Cordinate
genFoodPos (row,col) snakeCords = do
  x <- generate $ choose (0,row-1)
  y <- generate $ choose (0,col-1)
  let cord = Cord y x
  if Cord y x `elem` snakeCords then genFoodPos (row,col) snakeCords else return (Cord y x)
