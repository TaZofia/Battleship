module Draw where

import Graphics.Gloss
import Types
import Data.List (find)

-- Drawing
drawGame :: GameState -> Picture
drawGame (GameState sel phase placed current shipPlan hits aiShips aiGuesses turn rng aiTargets playerHitsTaken) =
  case phase of
    GameOver msg -> scale 0.3 0.3 $ translate (-400) 0 $ color black $ text msg
    _ -> pictures
      [ translate (-250) 0 $ drawBoard placed current sel (phase == Placement) "Your Ships" aiGuesses playerHitsTaken False
      , translate (250) 0  $ drawBoard aiShips [] sel (phase == Battle) "Enemy Board" hits (filter (`elem` concatMap tiles aiShips) hits) True
      , translate (-280) (-260) $ drawPhaseText phase turn shipPlan
      ]

-- Drawing single board (player or AI)
drawBoard
  :: [Ship]          -- ships list (player's or AI)
  -> [(Int, Int)]    -- ship while placing
  -> (Int, Int)      -- selected field
  -> Bool            -- highlight selection?
  -> String          -- board title
  -> [(Int, Int)]    -- enemy guesses (shown as yellow squares)
  -> [(Int, Int)]    -- successful hits (used to highlight sunk/green tiles)
  -> Bool            -- hide ships (used for AI board)
  -> Picture         -- result
drawBoard ships temp sel highlight title enemyHits targets hideShips =
  let
    allShipTiles = concatMap tiles ships
    allTiles = allShipTiles ++ temp

    sunkShips = filter (\ship -> all (`elem` targets) (tiles ship)) ships
    sunkTiles = concatMap tiles sunkShips

    -- Field coloring
    cellColor (x, y)
      | (x, y) `elem` temp = azure
      | (x, y) `elem` sunkTiles = orange
      | (x, y) `elem` allShipTiles && (x, y) `elem` targets = green
      | hideShips && (x, y) `elem` allShipTiles = black
      | (x, y) `elem` allShipTiles = red
      | (x, y) `elem` targets = red
      | otherwise = black

  in pictures $
    [ translate 0 220 $ scale 0.15 0.15 $ color black $ text title ] ++
    [ translate x' y' $ pictures
        [ color (cellColor (x,y)) $ rectangleSolid cellSize cellSize
        , if (x,y) `elem` enemyHits
            then color yellow (rectangleSolid (cellSize / 2) (cellSize / 2))
            else blank
        , if highlight && sel == (x,y)
            then drawGreenOutline
            else color white $ rectangleWire cellSize cellSize
        ]
    | x <- [0 .. gridSize - 1], y <- [0 .. gridSize - 1]
    , let x' = fromIntegral x * cellSize - (cellSize * fromIntegral gridSize) / 2 + cellSize / 2
          y' = fromIntegral y * cellSize - (cellSize * fromIntegral gridSize) / 2 + cellSize / 2
    ]

-- Green outline for selected field
drawGreenOutline :: Picture
drawGreenOutline = color green $ pictures
  [ translate 0 (cellSize / 2 - lw / 2) $ rectangleSolid cellSize lw
  , translate 0 (-cellSize / 2 + lw / 2) $ rectangleSolid cellSize lw
  , translate (cellSize / 2 - lw / 2) 0 $ rectangleSolid lw cellSize
  , translate (-cellSize / 2 + lw / 2) 0 $ rectangleSolid lw cellSize
  ]
  where lw = 4

-- Drawing helper text
drawPhaseText :: Phase -> Turn -> [Int] -> Picture
drawPhaseText phase turn shipPlan = scale 0.15 0.15 . color black . text $
  case phase of
    Placement -> "Placement Phase: Place ships " ++ show shipPlan ++ " (Enter=Add, Tab=Confirm, Backspace=Reset)"
    Battle    -> "Battle Phase - Turn: " ++ showTurn turn
    GameOver _ -> "Game Over"
  where
    showTurn PlayerTurn = "Player"
    showTurn AITurn     = "AI"
