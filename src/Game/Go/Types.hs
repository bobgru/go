module Game.Go.Types where

import Data.Map (Map)

type Dim = Int
type Row = Dim
type Col = Dim
type Intersection = (Row, Col)
data Player = Black | White deriving (Show, Eq)
data Play = Move Intersection | Pass deriving (Show, Eq)
type Turn = (Player, Play)
type Position = Map Intersection Player
-- TODO add num black and white capture
-- turn into monad(s)?
type Game = (Dim, Position, [(Position, Turn)])
type Rank = Int

