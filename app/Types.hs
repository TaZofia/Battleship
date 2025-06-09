module Types where

import Graphics.Gloss
import System.Random (StdGen)

-- Constants
windowWidth, windowHeight :: Int
windowWidth = 1300
windowHeight = 600

cellSize :: Float
cellSize = 40

gridSize :: Int
gridSize = 10

-- Game phases and turn
data Phase = Placement | Battle | GameOver String deriving (Eq)
data Turn = PlayerTurn | AITurn deriving (Eq)

-- Ship type, contains list of fields
data Ship = Ship { tiles :: [(Int, Int)] } deriving (Show, Eq)

-- Game state
data GameState = GameState
  { selected     :: (Int, Int)      -- current field
  , phase        :: Phase
  , placedShips  :: [Ship]          -- player's ships
  , currentShip  :: [(Int, Int)]
  , shipPlan     :: [Int]
  , hits         :: [(Int, Int)]
  , aiShips      :: [Ship]
  , aiGuesses    :: [(Int, Int)]
  , turn         :: Turn
  , rng          :: StdGen
  , aiTargets    :: [(Int, Int)]
  , playerHitsTaken :: [(Int, Int)]  -- AI hits player
  }

-- Initial game state
initialState :: StdGen -> GameState
initialState gen = GameState
  { selected     = (0, 0)
  , phase        = Placement
  , placedShips  = []
  , currentShip  = []
  , shipPlan     = [4,3,3,2,2,2,1,1,1,1]
  , hits         = []
  , aiShips      = []
  , aiGuesses    = []
  , turn         = PlayerTurn
  , rng          = gen
  , aiTargets    = []
  , playerHitsTaken = []
  }
