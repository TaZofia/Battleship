module Types where

import Graphics.Gloss

-- Constants
windowWidth, windowHeight :: Int
windowWidth = 1200
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
  }

-- Initial game state
initialState :: GameState
initialState = GameState
  { selected     = (0, 0)
  , phase        = Placement
  , placedShips  = []
  , currentShip  = []
  , shipPlan     = [4,3,3,2,2,2,1,1,1,1]    -- standard ships requirement
  , hits         = []
  , aiShips      = []
  , aiGuesses    = []
  , turn         = PlayerTurn
  }

-- Mock ships for AI
mockAIShips :: [Ship]
mockAIShips =
  [ Ship { tiles = [(0,0), (0,1), (0,2), (0,3)] }
  , Ship { tiles = [(2,0), (2,1), (2,2)] }
  , Ship { tiles = [(4,0), (4,1)] }
  , Ship { tiles = [(6,0)] }
  , Ship { tiles = [(8,0)] }
  , Ship { tiles = [(8,2)] }
  , Ship { tiles = [(8,4)] }
  ]
