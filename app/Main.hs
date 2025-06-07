module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (nub)
import System.Random (StdGen, newStdGen, randomR)
import Types
import Input (touchingTiles, orthogonalNeighbors)  -- importujemy te funkcje z Input
import Draw
import Input (handleInput)  -- import handleInput

-- Funkcja sprawdzająca, czy pole jest na planszy
onBoard :: (Int, Int) -> Bool
onBoard (x,y) = x >= 0 && x < 10 && y >= 0 && y < 10

-- Sprawdza, czy statek można umieścić (nie zachodzi na inne statki ani ich pola przyległe)
validPlacement :: [(Int, Int)] -> [Ship] -> Bool
validPlacement shipTiles existing =
  length shipTiles == length (nub shipTiles) && -- brak powtórzeń
  all onBoard shipTiles &&
  not (any (`elem` allOccupiedOrTouching) shipTiles)
  where
    allOccupied = concatMap tiles existing
    allOccupiedOrTouching = allOccupied ++ concatMap touchingTiles allOccupied

-- Generuje pojedynczy statek "pozaginany" o zadanym rozmiarze
generateShip :: Int -> [Ship] -> StdGen -> (Ship, StdGen)
generateShip size existing gen = go [] gen
  where
    go current g
      | length current == size = (Ship current, g)
      | null current = -- losowy start
          let (x, g1) = randomR (0,9) g
              (y, g2) = randomR (0,9) g1
          in go [(x,y)] g2
      | otherwise =
          let neighbors = filter (\p -> onBoard p && notElem p current && validPlacement (current ++ [p]) existing) (orthogonalNeighbors (last current))
          in if null neighbors
             then
               if length current == 1
                 then
                   let (x, g1) = randomR (0,9) g
                       (y, g2) = randomR (0,9) g1
                   in go [(x,y)] g2
                 else go (init current) g
             else
               let (idx, g3) = randomR (0, length neighbors - 1) g
                   nextPos = neighbors !! idx
               in go (current ++ [nextPos]) g3

-- Generuje listę statków AI wg planu rozmiarów
generateAIShips :: [Int] -> StdGen -> ([Ship], StdGen)
generateAIShips [] gen = ([], gen)
generateAIShips (size:sizes) gen =
  let (ship, gen1) = generateShip size [] gen
      (restShips, gen2) = generateAIShips sizes gen1
  in if validPlacement (tiles ship) restShips
     then (ship : restShips, gen2)
     else generateAIShips (size:sizes) gen2

-- Główna funkcja startująca grę
main :: IO ()
main = do
  gen <- newStdGen
  let (aiShips, gen') = generateAIShips [4,3,3,2,2,2,1,1,1,1] gen
      initial = (initialState gen') { aiShips = aiShips }
  play
    (InWindow "Battleships" (windowWidth, windowHeight) (100, 100))
    white
    30
    initial
    drawGame
    handleInput
    (\_ s -> s)
