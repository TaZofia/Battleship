module Draw where

import Graphics.Gloss
import Types
import Data.List (find)


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
