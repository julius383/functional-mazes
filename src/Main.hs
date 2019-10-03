module Main where

import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import Data.List

maze = mazeGraph 9

emptyEdges :: [UEdge]
emptyEdges = []

main :: IO ()
main = do
  let dot = showDot (fglToDot (asSquare [1, 1] [2 .. 9] maze))
  putStrLn "Writing dot file"
  writeFile "file.dot" dot

mazeGraph :: Int -> Gr Int ()
mazeGraph n = mkGraph (zip nodes nodes) emptyEdges
  where
    nodes = [1 .. n :: Node]

-- | Get all possible pairs of edges for a list of nodes
edgeCombinations :: [Node] -> [Edge]
edgeCombinations xs = concatMap f xs
  where
    f v = [(v, i) :: Edge | i <- filter (/= v) xs]

-- | True if given node has no edges in or out
noEdges :: Graph gr => gr a b -> LNode a -> Bool
noEdges g = (== 0) . deg g . fst

-- | Create an edge between two nodes
connect :: Node -> Node -> UEdge
connect x y = (x, y, ()) :: UEdge

{-|
   Chain connect a list of nodes e.g for [1, 2, 3, 4]
   1 is connected to 2, 2 to 3 and 3 to 4
-}
attach :: [Node] -> [UEdge]
attach (x:y:xs) = [connect x y] ++ attach (y : xs)
attach _ = []

{-|
  Project an empty graph into a square by creating
  edges between nodes representing neighbours in a
  grid
-}
asSquare :: DynGraph gr => [Node] -> [Node] -> gr a () -> gr a ()
asSquare _ [] g = g
asSquare toConnect unconnected g =
  asSquare (side1 ++ [corner] ++ side2 ++ [corner]) orphans newGraph
  where
    corner    = head unconnected
    adjGroups = splitAt (n `div` 2) $ take n (tail unconnected)
    side1     = fst adjGroups
    side2     = snd adjGroups
    bl        = attach $ side2 ++ [corner]
    bu        = attach $ side1 ++ [corner]
    adjCon    = zipWith connect toConnect (side1 ++ side2)
    newGraph  = insEdges (bl ++ bu ++ adjCon) g
    n         = length toConnect
    orphans   = map fst $ filter (noEdges newGraph) (labNodes newGraph)
