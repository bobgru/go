{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, TypeFamilies #-}
module Game.Go.Diagram
where

import Game.Go.Core hiding ((#))
import Game.Go.Types
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

boardDgm :: Dim -> Diagram B
boardDgm d = foldr1 (===) $ replicate d' row
  where
    row = foldr1 (|||) $ replicate d' (square 1)
    d' = d - 1

stone :: Player -> Diagram B
stone pl = circle 0.5 # fc (fillFromPlayer pl)

fillFromPlayer :: Player -> Colour Double
fillFromPlayer Black = black
fillFromPlayer White = white

stoneAt :: Dim -> Intersection -> Diagram B -> Diagram B
stoneAt d (r,c) dg  = position [(p, dg)]
  where
    p :: Point V2 Double
    p = p2 (r', c')
    (r', c') = (fromIntegral c - 0.5, (- fromIntegral r) + 0.5)

example =  stoneAt 9 (0,0) (stone Black)
        <> stoneAt 9 (0,1) (stone White)
        <> stoneAt 9 (1,0) (stone Black)
        <> boardDgm 9

