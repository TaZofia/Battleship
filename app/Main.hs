module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (nub, delete, find)
import Data.Maybe (isJust)

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

-- Main entry
main :: IO ()
main = play         -- play: Gloss function, which starts a game
  (InWindow "Battleships" (windowWidth, windowHeight) (100, 100))
  white
  30
  initialState
  drawGame
  handleInput       -- player's input handling
  (\_ s -> s)

-- Drawing
drawGame :: GameState -> Picture
drawGame (GameState sel phase placed current shipPlan hits aiShips aiGuesses turn) =
  case phase of
    GameOver msg -> scale 0.3 0.3 $ translate (-400) 0 $ color black $ text msg
    _ -> pictures
      [ translate (-250) 0 $ drawBoard placed current sel (phase == Placement) "Your Ships" aiGuesses [] False
      , translate (250) 0  $ drawBoard aiShips [] sel (phase == Battle) "Enemy Board" hits (filter (`elem` concatMap tiles aiShips) hits) True
      , translate (-280) (-260) $ drawPhaseText phase turn shipPlan
      ]


lookupShipAt :: (Int, Int) -> [Ship] -> Maybe Ship
lookupShipAt pos = find (\ship -> pos `elem` tiles ship)


drawBoard
 :: [Ship]          -- ships list (player's or AI)
 -> [(Int, Int)]    -- ship while placing
 -> (Int, Int)      -- selected field
 -> Bool            -- ? highlight field ?
 -> String          -- board title
 -> [(Int, Int)]    -- enemy guesses (yellow)
 -> [(Int, Int)]    -- hits (green)
 -> Bool            -- hide ships? - AI case
 -> Picture         -- result

drawBoard ships temp sel highlight title enemyHits targets hideShips =
  let
    allShipTiles = concatMap tiles ships      -- list of fields with ships
    allTiles = allShipTiles ++ temp

    sunkShips = filter (\ship -> all (`elem` targets) (tiles ship)) ships
    sunkTiles = concatMap tiles sunkShips

    -- (x, y) - field
    cellColor (x, y)
      | (x, y) `elem` temp = azure
      | (x, y) `elem` sunkTiles = orange        -- sunk ships
      | (x, y) `elem` allShipTiles && (x, y) `elem` targets = green
      | hideShips && (x, y) `elem` allShipTiles = black         -- black field if it's AI board (we hide ships)
      | (x, y) `elem` allShipTiles = red        -- player's ship - filed red
      | (x, y) `elem` targets = red
      | otherwise = black
  in pictures $
    [ translate 0 220 $ scale 0.15 0.15 $ color black $ text title ] ++
    [ translate x' y' $ pictures        -- counting coordinates for each filed in Gloss window
        -- color as it was implemented in function cellColor
        [ color (cellColor (x,y)) $ rectangleSolid cellSize cellSize
        , if (x,y) `elem` enemyHits
            then color yellow (rectangleSolid (cellSize / 2) (cellSize / 2))
            else blank
        , if highlight && sel == (x,y)
            then drawGreenOutline
            else color white $ rectangleWire cellSize cellSize
        ]

    -- grid generator
    | x <- [0 .. gridSize - 1], y <- [0 .. gridSize - 1]
    , let x' = fromIntegral x * cellSize - (cellSize * fromIntegral gridSize) / 2 + cellSize / 2
          y' = fromIntegral y * cellSize - (cellSize * fromIntegral gridSize) / 2 + cellSize / 2
    ]


drawGreenOutline :: Picture
drawGreenOutline = color green $ pictures
  [ translate 0 (cellSize / 2 - lw / 2) $ rectangleSolid cellSize lw    -- frame upper line
  , translate 0 (-cellSize / 2 + lw / 2) $ rectangleSolid cellSize lw   -- frame bootom line
  , translate (cellSize / 2 - lw / 2) 0 $ rectangleSolid lw cellSize    -- frame right line
  , translate (-cellSize / 2 + lw / 2) 0 $ rectangleSolid lw cellSize   -- frame left line
  ]
  where lw = 4      -- line width 4 px

-- [Int] - list of ships to place
drawPhaseText :: Phase -> Turn -> [Int] -> Picture
drawPhaseText phase turn shipPlan = scale 0.15 0.15 . color black . text $     -- text - converts String to Picture
  case phase of
    Placement -> "Placement Phase: Place ships " ++ show shipPlan ++ " (Enter=Add, Tab=Confirm, Space=Start)"
    Battle    -> "Battle Phase - Turn: " ++ showTurn turn
    GameOver _ -> "Game Over"
  where
    showTurn PlayerTurn = "Player"
    showTurn AITurn     = "AI"


handleInput :: Event -> GameState -> GameState

-- when the game is over we ignore all keys and return game state without changes
handleInput _ gs@(GameState _ (GameOver _) _ _ _ _ _ _ _) = gs

handleInput (EventKey (SpecialKey key) Down _ _) gs@(GameState sel phase ships current plan hits aiShips aiGuesses turn) =
  case key of
    KeyUp    -> gs { selected = move sel (0, 1) }
    KeyDown  -> gs { selected = move sel (0, -1) }
    KeyLeft  -> gs { selected = move sel (-1, 0) }
    KeyRight -> gs { selected = move sel (1, 0) }

    KeyEnter ->
      if phase == Placement && sel `notElem` (concatMap tiles ships ++ current)
         then gs { currentShip = sel : current }
         else if phase == Battle && turn == PlayerTurn && sel `notElem` hits
            then
              let newHits = sel : hits
              in if all (`elem` newHits) (concatMap tiles aiShips)
                    then gs { phase = GameOver "You Win" }
                    else aiTurn $ gs { hits = newHits, turn = AITurn }
            else gs

    KeyTab ->
      if phase == Placement && not (null plan) && validShip (head plan) current ships
         then
            let newShips = Ship current : ships
                newPlan = tail plan
                newState = gs { placedShips = newShips, currentShip = [], shipPlan = newPlan }
            in if null newPlan
                 then newState { phase = Battle, selected = (0,0), aiShips = mockAIShips }  -- start game if last ship is placed
                 else newState
            else gs


    KeySpace ->
      if phase == Placement && null plan
         then gs { phase = Battle, selected = (0,0), aiShips = mockAIShips }
         else gs

    _ -> gs

-- Backspace
handleInput (EventKey (Char '\b') Down _ _) gs@(GameState sel Placement placed _ plan hits aiShips aiGuesses turn) =
  gs { currentShip = [] }

handleInput (EventKey (Char '\b') Down _ _) gs = gs

handleInput _ s = s     -- other? just ignore

-- helper function
--       field         shift      new position after shift
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x, y) (dx, dy) = (clamp 0 9 (x + dx), clamp 0 9 (y + dy))   -- clamp - ensures coordinates are between 0 and 9
clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

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

aiTurn :: GameState -> GameState
aiTurn gs@(GameState _ _ placed _ _ _ _ aiGuesses AITurn) =
  let allCoords = [ (x,y) | x <- [0..9], y <- [0..9] ]
      unused = filter (`notElem` aiGuesses) allCoords
      playerShipTiles = concatMap tiles placed
  in case unused of
       []    -> gs { turn = PlayerTurn }
       (a:_) ->
         let newGuesses = a : aiGuesses
         in if all (`elem` newGuesses) playerShipTiles
               then gs { aiGuesses = newGuesses, phase = GameOver "You Lose" }
               else gs { aiGuesses = newGuesses, turn = PlayerTurn }
aiTurn gs = gs

validShip :: Int -> [(Int, Int)] -> [Ship] -> Bool
validShip size tiles existing =
     length tiles == size
  && allAdjacent tiles
  && not (any (`elem` allOccupiedOrTouching) tiles)
  where
    allOccupied = concatMap Main.tiles existing
    allOccupiedOrTouching = allOccupied ++ concatMap touchingTiles allOccupied

allAdjacent :: [(Int, Int)] -> Bool
allAdjacent [] = False
allAdjacent (x:xs) = dfs [x] [] == length (nub (x:xs))
  where
    dfs [] visited = length visited
    dfs (t:ts) visited
      | t `elem` visited = dfs ts visited
      | otherwise = dfs (adjacentTo t (x:xs) ++ ts) (t:visited)
    adjacentTo (x,y) tiles = [ (a,b) | (a,b) <- tiles, (abs (x-a) + abs (y-b)) == 1 ]

touchingTiles :: (Int, Int) -> [(Int, Int)]
touchingTiles (x,y) = [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0),
                                       x+dx >= 0, x+dx < 10, y+dy >= 0, y+dy < 10 ]