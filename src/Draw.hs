{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Draw where

import Diagrams.Backend.Rasterific
import Diagrams.Prelude
import Diagrams.TwoD.Size
import Data.Colour.Names
import Data.Graph.Inductive
import Data.List
import Data.Maybe

import Maze

dimensions :: Num n => SizeSpec V2 n
dimensions = mkSizeSpec2D (Just 800) (Just 800)

ofile :: FilePath
ofile = "out.png"

draw = renderRasterific ofile dimensions

drawSquareMaze :: (DynGraph gr, Show a) => gr a Integer -> Diagram B
drawSquareMaze g = (border `atop` drawnCells) # centerXY # pad 1.1 where
  cells      = map (map (drawCell g)) nodesList
  nodesList  = map (spf g) (lpf g 1)
  drawnCells = vcat $ map hcat cells
  border     = boundingRect drawnCells # lw medium 

drawCell :: (DynGraph gr, Show a) => gr a Integer -> Node -> Diagram B
drawCell g n 
  = square 5 
  # explodeTrail
  # zipWith lc [right, top, left, bottom]
  # mconcat `atop` text label where
    label         = show $ fromJust $ lab g n
    (left, right) = (check 1 into, check 1 outof)
    (top, bottom) = (check 2 into, check 2 outof)
    into          = inn g n
    outof         = out g n
    check i xs    = case find (\(_, _, e) -> i == e) xs of
                   Nothing -> white
                   Just _ -> black
