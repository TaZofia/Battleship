module Input where

import Graphics.Gloss.Interface.Pure.Game
import Types
import Data.List (nub, find)
import System.Random (randomR)

handleInput :: Event -> GameState -> GameState

-- when the game is over we ignore all keys and return game state without changes
handleInput _ gs@(GameState _ (GameOver _) _ _ _ _ _ _ _ _ _ _ ) = gs

handleInput (EventKey (SpecialKey key) Down _ _) gs@(GameState sel phase ships current plan hits aiShips aiGuesses turn rng aiTargets playerHitsTaken) =
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
                 then newState { phase = Battle, selected = (0,0) }
                 else newState
         else gs

    KeySpace ->
      if phase == Placement && null plan
         then gs { phase = Battle, selected = (0,0) }
         else gs

    _ -> gs

-- Backspace
handleInput (EventKey (Char '\b') Down _ _) gs@(GameState sel Placement placed _ plan hits aiShips aiGuesses turn rng aiTargets playerHitsTaken) =
  gs { currentShip = [] }

handleInput (EventKey (Char '\b') Down _ _) gs = gs

handleInput _ s = s

-- helper function
move :: (Int, Int) -> (Int, Int) -> (Int, Int)
move (x, y) (dx, dy) = (clamp 0 9 (x + dx), clamp 0 9 (y + dy))
clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

-- AI TURN
aiTurn :: GameState -> GameState
aiTurn gs@(GameState sel phase placed current plan hits aiShips aiGuesses AITurn rng aiTargets playerHitsTaken) =
  let allCoords = [(x,y) | x <- [0..9], y <- [0..9]]
      playerShipTiles = concatMap tiles placed
      unused = filter (`notElem` aiGuesses) allCoords

      -- Wybór celu: z aiTargets lub losowo
      (target, remainingTargets, newRng) = case filter (`notElem` aiGuesses) aiTargets of
        (t:ts) -> (t, ts, rng)
        [] -> let (index, r) = randomR (0, length unused - 1) rng
              in (unused !! index, [], r)

      newGuesses = target : aiGuesses
      isHit = target `elem` playerShipTiles
      updatedHits = if isHit then nub (target : playerHitsTaken) else playerHitsTaken

      maybeHitShip = find (\s -> target `elem` tiles s) placed
      isSunk = case maybeHitShip of
        Just ship -> all (`elem` updatedHits) (tiles ship)
        Nothing -> False

      currentTargetCluster =
        case maybeHitShip of
          Just ship | not isSunk -> filter (`elem` updatedHits) (tiles ship)
          _ -> []

      potentialTargets = concatMap orthogonalNeighbors currentTargetCluster

      newTargetsFromHit =
        if isHit && not isSunk
           then filter (`notElem` (newGuesses ++ remainingTargets)) potentialTargets
           else []

      updatedTargets
        | isSunk     = []
        | isHit      = remainingTargets ++ newTargetsFromHit
        | otherwise  = remainingTargets

  in if null unused
       then gs { turn = PlayerTurn }
       else if all (`elem` newGuesses) playerShipTiles
              then gs { aiGuesses = newGuesses, playerHitsTaken = updatedHits, phase = GameOver "You Lose", rng = newRng }
              else gs
                { aiGuesses = newGuesses
                , playerHitsTaken = updatedHits
                , aiTargets = updatedTargets
                , turn = PlayerTurn
                , rng = newRng
                }

aiTurn gs = gs

-- Sąsiedzi ortogonalni
orthogonalNeighbors :: (Int, Int) -> [(Int, Int)]
orthogonalNeighbors (x,y) = filter onBoard [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
  where
    onBoard (a,b) = a >= 0 && a < 10 && b >= 0 && b < 10

-- Walidacja statku
validShip :: Int -> [(Int, Int)] -> [Ship] -> Bool
validShip size shipTiles existing =
     length shipTiles == size
  && allAdjacent shipTiles
  && not (any (`elem` allOccupiedOrTouching) shipTiles)
  where
    allOccupied = concatMap tiles existing
    allOccupiedOrTouching = allOccupied ++ concatMap touchingTiles allOccupied

-- Czy pola statku są sąsiadujące
allAdjacent :: [(Int, Int)] -> Bool
allAdjacent [] = False
allAdjacent (x:xs) = dfs [x] [] == length (nub (x:xs))
  where
    dfs [] visited = length visited
    dfs (t:ts) visited
      | t `elem` visited = dfs ts visited
      | otherwise = dfs (adjacentTo t (x:xs) ++ ts) (t:visited)
    adjacentTo (x,y) tiles = [ (a,b) | (a,b) <- tiles, (abs (x-a) + abs (y-b)) == 1 ]

-- Pola przyległe
touchingTiles :: (Int, Int) -> [(Int, Int)]
touchingTiles (x,y) = [ (x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], (dx,dy) /= (0,0),
                                       x+dx >= 0, x+dx < 10, y+dy >= 0, y+dy < 10 ]
