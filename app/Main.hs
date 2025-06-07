module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (nub, delete, find)
import Data.Maybe (isJust)
import System.Random (newStdGen)
import Types
import Input
import Draw

-- Main entry
main :: IO ()
main = do
  gen <- newStdGen
  play              -- Gloss function which starts a game
    (InWindow "Battleships" (windowWidth, windowHeight) (100, 100))
    white
    30
    (initialState gen)
    drawGame
    handleInput
    (\_ s -> s)



