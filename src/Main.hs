module Main where

import Data.List
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot

maze = (mazeGraph 7)

main :: IO ()
main = do
  let dot = showDot (fglToDot maze)
  putStrLn "Writing dot file"
  writeFile "file.dot" dot

labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(i,j) -> (i,j,()))

mazeGraph :: Int -> Gr Int ()
mazeGraph n = mkGraph (zip nodes nodes) (labUEdges (edgeCombinations nodes)) where
  nodes = [1..n :: Node]

edgeCombinations :: [Node] -> [Edge]
edgeCombinations xs = concatMap f xs where
  f v = [(v, i) :: Edge| i <- filter (/= v) xs]


