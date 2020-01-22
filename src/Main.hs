module Main where

import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import Data.List
import Debug.Trace
import Text.PrettyPrint
import Test.QuickCheck

import Maze
import Draw
import Algos

main :: IO ()
main = do
  let maze = mazeGraph 256
      myMaze = asSquare maze 16 16
  gened <- generate $ binaryTree myMaze South East
  draw (drawSquareMaze gened)
  prettyPrint gened
  prettyPrint myMaze
