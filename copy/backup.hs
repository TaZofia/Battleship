
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (nub, delete)
import Data.Maybe (isJust)

-- Constants
windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 600
cellSize :: Float
cellSize = 40
gridSize :: Int
gridSize = 10

-- Game phases and turn
data Phase = Placement | Battle | GameOver String deriving (Eq)
data Turn = PlayerTurn | AITurn deriving (Eq)

-- Ship type
data Ship = Ship { tiles :: [(Int, Int)] } deriving (Show, Eq)

-- Game state
data GameState = GameState
  { selected     :: (Int, Int)
  , phase        :: Phase
  , placedShips  :: [Ship]
  , currentShip  :: [(Int, Int)]
  , shipPlan     :: [Int]
  , hits         :: [(Int, Int)]
  , aiShips      :: [(Int, Int)]
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
  , shipPlan     = [4,3,3,2,2,2,1,1,1,1]
  , hits         = []
  , aiShips      = []
  , aiGuesses    = []
  , turn         = PlayerTurn
  }

-- Main entry
main :: IO ()
main = play
  (InWindow "Battleships" (windowWidth, windowHeight) (100, 100))
  white
  30
  initialState
  drawGame
  handleInput
  (\_ s -> s)

-- Drawing
drawGame :: GameState -> Picture
drawGame (GameState sel phase placed current shipPlan hits aiShips aiGuesses turn) =
  case phase of
    GameOver msg -> scale 0.3 0.3 $ translate (-400) 0 $ color black $ text msg
    _ -> pictures
      [ translate (-250) 0 $ drawBoard placed current sel (phase == Placement) "Your Ships" aiGuesses [] False
      , translate (250) 0  $ drawBoard [] [] sel (phase == Battle) "Enemy Board" hits (filter (`elem` aiShips) hits) True
      , translate (-280) (-260) $ drawPhaseText phase turn shipPlan
      ]

drawBoard :: [Ship] -> [(Int, Int)] -> (Int, Int) -> Bool -> String -> [(Int, Int)] -> [(Int, Int)] -> Bool -> Picture
drawBoard ships temp sel highlight title enemyHits targets hideShips =
  let
    allShipTiles = concatMap tiles ships
    allTiles = allShipTiles ++ temp
    cellColor (x, y)
      | (x, y) `elem` temp = azure
      | (x, y) `elem` allShipTiles && (x, y) `elem` targets = green
      | hideShips && (x, y) `elem` allShipTiles = black
      | (x, y) `elem` allShipTiles = red
      | (x, y) `elem` targets = red
      | otherwise = black
  in pictures $
    [ translate 0 220 $ scale 0.15 0.15 $ color black $ text title ] ++
    [ translate x' y' $ pictures
        [ color (cellColor (x,y)) $ rectangleSolid cellSize cellSize
        , if (x,y) `elem` enemyHits then color yellow (rectangleSolid (cellSize / 2) (cellSize / 2)) else blank
        , if highlight && sel == (x,y)
            then drawGreenOutline
            else color white $ rectangleWire cellSize cellSize
        ]
    | x <- [0 .. gridSize - 1], y <- [0 .. gridSize - 1]
    , let x' = fromIntegral x * cellSize - (cellSize * fromIntegral gridSize) / 2 + cellSize / 2
          y' = fromIntegral y * cellSize - (cellSize * fromIntegral gridSize) / 2 + cellSize / 2
    ]

drawGreenOutline :: Picture
drawGreenOutline = color green $ pictures
  [ translate 0 (cellSize / 2 - lw / 2) $ rectangleSolid cellSize lw
  , translate 0 (-cellSize / 2 + lw / 2) $ rectangleSolid cellSize lw
  , translate (cellSize / 2 - lw / 2) 0 $ rectangleSolid lw cellSize
  , translate (-cellSize / 2 + lw / 2) 0 $ rectangleSolid lw cellSize
  ]
  where lw = 4

drawPhaseText :: Phase -> Turn -> [Int] -> Picture
drawPhaseText phase turn shipPlan = scale 0.15 0.15 . color black . text $
  case phase of
    Placement -> "Placement Phase: Place ships " ++ show shipPlan ++ " (Enter=Add, Tab=Confirm, Space=Start)"
    Battle    -> "Battle Phase - Turn: " ++ showTurn turn
    GameOver _ -> "Game Over"
  where
    showTurn PlayerTurn = "Player"
    showTurn AITurn     = "AI"

handleInput :: Event -> GameState -> GameState
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
              in if all (`elem` newHits) aiShips
                    then gs { phase = GameOver "You Win" }
                    else aiTurn $ gs { hits = newHits, turn = AITurn }
            else gs

    KeyTab ->
      if phase == Placement && not (null plan) && validShip (head plan) current ships
         then gs { placedShips = Ship current : ships
                 , currentShip = []
                 , shipPlan = tail plan
                 }
         else gs

    KeySpace ->
      if phase == Placement && null plan
         then gs { phase = Battle, selected = (0,0), aiShips = mockAIShips }
         else gs

    _ -> gs

handleInput (EventKey (Char '\b') Down _ _) gs@(GameState sel Placement placed _ plan hits aiShips aiGuesses turn) =
  gs { currentShip = [] }

handleInput (EventKey (Char '\b') Down _ _) gs = gs

handleInput _ s = s

move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x, y) (dx, dy) = (clamp 0 9 (x + dx), clamp 0 9 (y + dy))
clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

mockAIShips :: [(Int, Int)]
mockAIShips = [(0,0),(0,1),(0,2),(0,3), (2,0),(2,1),(2,2), (4,0),(4,1), (6,0),(8,0),(8,2),(8,4)]

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
