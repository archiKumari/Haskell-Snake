{-# Language TemplateHaskell #-}

module Types where

import Brick.Types

data Snake = Snake 
  { sHead :: Cordinate
  , sTail :: [Cordinate]
  , sDir  :: DIRECTION
  }
  deriving (Eq,Ord)

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

data Cordinate = Cord 
  { xCord :: Int
  , yCord :: Int
  }
  deriving (Show,Eq,Ord)

data DIRECTION = UP | DOWN | RIGHT | LEFT
  deriving (Eq,Ord)

data Mode = Normal | Infinite | Obstacle
  deriving (Show,Eq,Ord)

data GameStatus = Initial | ModeSelect | Playing | Paused | GameOver
  deriving (Show,Eq,Ord)

type GameSize = (Int,Int)

suffixLenses ''GameState
suffixLenses ''Snake