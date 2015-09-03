{-# LANGUAGE NoMonomorphismRestriction #-}
module Main
where
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Game.Go.Diagram

main = mainWith $ (example # centerXY # pad 1.2)
