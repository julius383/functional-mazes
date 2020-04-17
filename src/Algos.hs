module Algos where

import Maze
import Data.Graph.Inductive
import Data.Maybe
import Data.List (find, any, nub)
import Data.Tuple (fst, snd)
import Data.Set (Set)
import qualified Data.Set as S
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
  g'       = delEdges (map toEdge xs) g
  newEdges = map (\(x, y, z) -> (x, y, negate $ abs z)) xs

binaryTree :: DynGraph gr => gr a Integer -> Cardinal -> Cardinal -> Gen (gr a Integer)
binaryTree g a b = do 
  choices <- infiniteListOf $ elements [a, b]
  let opp c = if c == a then b else a
  let removeWall c n = case cardNeighbours g c n  of
                 [] -> case cardNeighbours g (opp c) n of
                         [] -> Nothing
                         xs -> Just (head xs)
                 xs -> Just (head xs)
  let orderedCells = concatMap (spf g) (lpf g 1)
      toRemove     = zipWith removeWall choices orderedCells
      nonMaybe     = filter isJust toRemove
      edgeList     = map fromJust nonMaybe
  return $ removeEdges g edgeList


randomWalk :: DynGraph gr => gr a Integer -> Node -> [Node] -> Gen [LEdge Integer]
randomWalk g n visited = do
  next <- elements (neighbors g n)
  if S.member next (S.fromList visited) || S.fromList (nodes g) == S.fromList visited
     then return ([] :: [LEdge Integer])
     else do 
       let linkEdge = find (\(x, y, _) -> (x == n && y == next) || x == next && y == n) (labEdges g)
       let new = fromJust linkEdge
       res <- randomWalk g next (next : visited)
       return (new : res)


huntandkill :: DynGraph gr => gr a Integer -> Gen (gr a Integer)
huntandkill g = do
  let visitedNeighbour vs x = S.member x (S.fromList vs)
  let findNext g visited = find pred (nodes g) where
        pred x = S.notMember x (S.fromList visited) && any (visitedNeighbour visited) (neighbors g x)
  let go g curr visited = do
        edges <- randomWalk g curr visited -- kill mode
        let newVisited = nub $ visited ++ (map (\(x, _, _) -> x) edges ++ map (\(_, y, _)-> y) edges)
        let g' = removeEdges g edges
        case findNext g' newVisited of -- hunt mode
          Nothing -> return g'
          Just next -> do
            let findLink (x, y, _) = (x == next && S.member y (S.fromList newVisited)) || (y == next  && S.member x (S.fromList newVisited)) 
            let link = case find findLink (inn g' next ++ out g' next) of
                         Nothing -> []
                         Just n -> [n]
            let g'' = removeEdges g' link
            go g'' next (curr : newVisited )
  begin <- elements (nodes g)
  go g begin [] 
