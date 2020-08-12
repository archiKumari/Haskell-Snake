{-# Language TemplateHaskell #-}

module Types where

import Brick.Types

-- | Data Type to represent snake
data Snake = Snake 
  { sHead :: Cordinate
  , sTail :: [Cordinate]
  , sDir  :: DIRECTION
  }
  deriving (Eq,Ord)

-- | Data Type to represent GameState
data GameState = GameState
  { gsSnake      :: Snake
  , gsSize       :: GameSize
  , gsFoodPos    :: Cordinate
  , gsFoodCount  :: Int
  , gsLifeCount  :: Int
  , gsScore      :: Int
  , gsHighScore  :: Int
  , gsLevel      :: Int
  , gsMode       :: Mode
  , gsGameStatus :: GameStatus
  }
  deriving (Eq,Ord)

-- | Data Type to represent the cordinate of grid
data Cordinate = Cord 
  { xCord :: Int
  , yCord :: Int
  }
  deriving (Show,Eq,Ord)

-- | Data Type to represent the direction of snake movement
data DIRECTION = UP | DOWN | RIGHT | LEFT
  deriving (Eq,Ord)

-- | Data Type to represent the different modes of game
data Mode = Normal | Infinite | Obstacle
  deriving (Show,Eq,Ord)

-- | Data Type to represent the different stages of Game Play
data GameStatus = Initial | ModeSelect | Playing | Paused | GameOver
  deriving (Show,Eq,Ord)

-- Type to represent the size of grid for the game
type GameSize = (Int,Int)

suffixLenses ''GameState
suffixLenses ''Snake