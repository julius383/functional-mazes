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
  let maze = mazeGraph 400
      myMaze = asSquare maze 20 20
  gened <- generate $ huntandkill myMaze 
  draw (drawSquareMaze gened)
  let dot = showDot (fglToDot myMaze)
  writeFile "file.dot" dot
  prettyPrint gened
  prettyPrint myMaze
