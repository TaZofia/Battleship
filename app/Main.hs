module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.List (nub, delete, find)
import Data.Maybe (isJust)
import Types
import Input
import Draw

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



