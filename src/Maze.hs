module Maze where

import Data.Graph.Inductive
import Data.Graph.Inductive.Dot
import Data.List
import Debug.Trace
import Text.PrettyPrint


emptyEdges :: [LEdge Integer]
emptyEdges = []

mazeGraph :: Int -> Gr Int Integer
mazeGraph n = mkGraph (zip nodes nodes) emptyEdges
  where
    nodes = [1 .. n :: Node]

-- | Get all possible pairs of edges for a list of nodes
edgeCombinations :: [Node] -> [Edge]
edgeCombinations xs = concatMap f xs
  where
    f v = [(v, i) :: Edge | i <- filter (/= v) xs]

-- | True if given node has no edges in or out
noEdges :: Graph gr => gr a b -> Node -> Bool
noEdges g = (== 0) . deg g

-- | Create an edge between two nodes
connect :: Integer -> Node -> Node -> LEdge Integer
connect n x y = (x, y, n)

{-|
   Chain connect a list of nodes e.g for [1, 2, 3, 4]
   1 is connected to 2, 2 to 3 and 3 to 4
-}
link :: Integer -> [Node] -> [LEdge Integer]
link n (x:y:xs) = connect n x y : link n (y : xs)
link _ _ = []

{-|
  Project an empty graph into a square by creating
  edges between nodes representing neighbours in a
  grid
-}
asSquare :: DynGraph gr => gr a Integer -> Int -> Int -> gr a Integer 
asSquare g 1 1 = merge g sg where
  sg = subgraph [n] g
  n = head $ filter (noEdges g) (nodes g) :: Node
asSquare g l w = merge g (asSquare newGraph (l - 1) (w - 1)) where
  entry       = head orphans
  avail       = tail orphans
  horiz       = take (l - 1) avail
  vert        = (take (w - 1) . drop (l - 1)) avail
  linkedHoriz = link 1 $ entry : horiz
  linkedVert  = link 2 $ entry :  vert
  newGraph    = insEdges (linkedVert ++ linkedHoriz) g
  orphans     = filter (noEdges g) (nodes g)


merge :: DynGraph gr  => gr a Integer -> gr a Integer -> gr a Integer
merge outer inner
  | all (noEdges outer) (nodes outer) = inner -- outermost case
  | otherwise =
    insEdges  (vert ++ horiz ++ innerCons) outer
  where
    innerHoriz = tail $ spf inner innerEntry
    innerVert = tail $ lpf inner innerEntry
    innerEntry =
      case find (\x -> null (inn outer x) && null (out outer x)) (nodes outer) of
        Just x -> x
    outerHoriz = tail $ spf outer outerEntry
    outerVert = tail $ lpf outer outerEntry
    outerEntry =  x
      where
        x =
          last $
          filter
            (\x -> length (inn outer x) == 2 || length (out outer x) == 2)
            (nodes outer)
    -- diag = connect 1 (head outerVert) innerEntry
    vert = zipWith (connect 2) outerHoriz (innerEntry : innerHoriz)
    horiz = zipWith (connect 1) outerVert (innerEntry : innerVert)
    innerCons = labEdges inner \\ labEdges outer

minByEdge :: DynGraph gr => gr a Integer -> Node -> Node
minByEdge g node = fst $ minimumBy (\(_,a) (_,b) -> compare a b) $ lsuc g node

maxByEdge :: DynGraph gr => gr a Integer -> Node -> Node
maxByEdge g node = fst $ maximumBy (\(_,a) (_,b) -> compare a b) $ lsuc g node

follow  :: DynGraph gr => gr a Integer -> Node -> Integer ->[Node]
follow g n i = case  nnode of
                 Nothing -> []
                 Just (x, _) -> x : follow g x i
               where
                 nnode = find (\(_,l) -> abs l == i) (lsuc g n)

-- Shortest path first traverses along single row
spf :: DynGraph gr => gr a Integer -> Node -> [Node]
spf g cur = cur : follow g cur 1

-- longest path first traverses along single column
lpf :: DynGraph gr => gr a Integer -> Node -> [Node]
lpf g cur = cur : follow g cur 2

-- FIXME
vizRow ::  (DynGraph gr, Show a) => gr a Integer -> Node -> Doc
vizRow g s = front <+> rest where
  front =  text (show s) <+> text "->"
  neighbours = spf g s
  rest = hsep $ intersperse (text "->") $ map (text . show) neighbours

visualize :: (DynGraph gr, Show a) => gr a Integer -> String
visualize g = render $ vcat (map (vizRow g) rows) <+> text "\n"
  where
    rows = start : lpf g start
    start = head $ nodes g
