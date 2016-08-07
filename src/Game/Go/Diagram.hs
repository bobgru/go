{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, TypeFamilies #-}
module Game.Go.Diagram
where

import Game.Go.Core hiding ((#))
import Game.Go.Types
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.List (intersperse)
import Data.Map (toList)

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

stonesAt :: [(Location, Diagram B)] -> Diagram B
stonesAt pss = position $ map (\(p, d) -> (boardToDiagram p, d)) pss

stoneAt :: Location -> Diagram B -> Diagram B
stoneAt (r,c) dg  = stonesAt [((r,c), dg)]

-- Convert a location to its location in the diagram vector space.
boardToDiagram :: Location -> Point V2 Double
boardToDiagram (r, c) = p2 (fromIntegral c - 0.5, (- fromIntegral r) + 0.5)


b = stone Black
w = stone White

stonesFromState :: State -> [(Location, Diagram B)]
stonesFromState pn = map (\(i, pl) -> (i, stone pl)) $ toList pn

g00 = initGame 9 3
g01 = addTurn (White, Move (0, 0)) g00

g02 = sequenceTurns g00 White [
       (0,0) 
     , (0,1)
     , (1,0)
     , (0,8)
     , (4,4)
     , (8,0)
     , (8,8)
    ]

p00 = sequenceMoves (currentState g00) White [
       (0,0) 
     , (0,1)
     , (1,0)
     , (0,8)
     , (4,4)
     , (8,0)
     , (8,8)
    ]

stones :: [(Location, Diagram B)]
stones = stonesFromState p00

history :: Game -> [Diagram B]
history (d, _, pnts) = zipWith (<>) ss bs
  where
    ss = map (stonesAt . stonesFromState . fst) pnts
    bs = repeat (boardDgm d)
 
exampleOld =  stonesAt stones
        <> boardDgm 9

example =  foldr1 (===) (intersperse (strutY 1.0) (history g02))
