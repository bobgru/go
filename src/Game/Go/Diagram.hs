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

stonesAt :: [(Intersection, Diagram B)] -> Diagram B
stonesAt pss = position $ map (\(p, d) -> (boardToDiagram p, d)) pss

stoneAt :: Intersection -> Diagram B -> Diagram B
stoneAt (r,c) dg  = stonesAt [((r,c), dg)]

boardToDiagram :: Intersection -> Point V2 Double
boardToDiagram (r, c) = p2 (fromIntegral c - 0.5, (- fromIntegral r) + 0.5)


b = stone Black
w = stone White

stones = [ ((0,0), b)
         , ((0,1), w)
         , ((1,0), b)
         , ((0,8), w)
         , ((4,4), b)
         , ((8,0), w)
         , ((8,8), w)
         ]

example =  stonesAt stones
        <> boardDgm 9

