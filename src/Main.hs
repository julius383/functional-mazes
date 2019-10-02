module Main where

import Data.List
import Data.Graph.Inductive
import Data.Graph.Inductive.Dot

maze = mazeGraph 7

emptyEdges :: [UEdge]
emptyEdges = []

main :: IO ()
main = do
  let dot = showDot (fglToDot maze)
  putStrLn "Writing dot file"
  writeFile "file.dot" dot

labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(i,j) -> (i,j,()))

mazeGraph :: Int -> Gr Int ()
mazeGraph n = mkGraph (zip nodes nodes) emptyEdges where
  nodes = [1..n :: Node]

edgeCombinations :: [Node] -> [Edge]
edgeCombinations xs = concatMap f xs where
  f v = [(v, i) :: Edge | i <- filter (/= v) xs]

noEdges :: Graph gr => gr a b -> LNode a -> Bool
noEdges g  = (== 0) . (deg g) . fst

connect ::  Node -> Node -> UEdge
connect x y = (x, y, ()) :: UEdge

asSquare :: DynGraph gr => [Node] -> gr a () -> gr a ()
asSquare [] g = g
asSquare (top:[]) g = insEdges (topConnections ++ bottomConnection) g where
  selected = map fst $ take (1 * 2) $ filter (noEdges g) (labNodes g)
  topConnections = (map ((connect top) . getNode) selected)
  bottomCorner =  fst $ head $ drop 2 $ filter (noEdges g) (labNodes g)
  bottomConnection = (map ((connect bottomCorner) . getNode) selected)
  getNode = node' . (context g)

byn :: Int -> (Int -> Bool)
byn n = (== 0) . (`mod` n)

