module Algos where

import Maze
import Data.Graph.Inductive
import Data.Maybe
import Test.QuickCheck

data Cardinal
  = North
  | East
  | South
  | West
  deriving (Eq, Show, Enum, Bounded)

cardNeighbours :: DynGraph gr => gr a Integer -> Cardinal -> Node -> [LEdge Integer]
cardNeighbours g c n = case c of
                       North -> filter (f 2) $ inn g n
                       East  -> filter (f 1) $ out g n
                       South -> filter (f 2) $ out g n
                       West  -> filter (f 1) $ inn g n
                     where f x e = edgeLabel e == x

removeEdges :: DynGraph gr => gr a Integer -> [LEdge Integer] -> gr a Integer
removeEdges g xs = insEdges newEdges g' where
  g' = delEdges (map toEdge xs) g
  newEdges = map (\(x, y, z) -> (x, y, negate z)) xs

binaryTree :: DynGraph gr => gr a Integer -> Cardinal -> Cardinal -> Gen (gr a Integer)
binaryTree g a b = do 
  choices <- infiniteListOf $ elements [a, b]
  let opp c = if c == a then b else a
  let removeWall c n = case cardNeighbours g c n  of
                 [] -> case cardNeighbours g (opp c) n of
                         [] -> Nothing
                         xs -> Just (head xs)
                 xs -> Just (head xs)
  let orderedCells  = concatMap (spf g) (lpf g 1)
      toRemove = zipWith removeWall choices orderedCells
      nonMaybe = filter isJust toRemove
      edgeList = map fromJust nonMaybe
  return $ removeEdges g edgeList

