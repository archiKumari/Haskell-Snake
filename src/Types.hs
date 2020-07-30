{-# Language TemplateHaskell #-}

module Types where

import Brick.Types

data Snake = Snake 
  { sHead :: Cordinate
  , sTail :: [Cordinate]
  , sDir :: DIRECTION
  }
  deriving (Eq,Ord)

data GameState = GameState
  { gsSnake :: Snake
  , gsSize :: (Int,Int)
  , gsFoodPos :: Cordinate
  , gsFoodCount :: Int
  , gsCoinCount :: Int
  , gsLifeCount :: Int
  , gsScore :: Int
  , gsHighScore :: Int
  , gsLevel :: Int
  }
  deriving (Eq,Ord)

data Cordinate = Cord 
  { xCord :: Int
  , yCord :: Int
  }
  deriving (Show,Eq,Ord)

data DIRECTION = UP | DOWN | RIGHT | LEFT
  deriving (Eq,Ord)

suffixLenses ''GameState
suffixLenses ''Snake