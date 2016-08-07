module Game.Go.Types where

import Data.Map (Map)

type Dim = Int
type Row = Dim
type Col = Dim
type Location = (Row, Col)
data Player = Black | White deriving (Show, Eq, Enum, Bounded)
data Move = Move Location | Pass deriving (Show, Eq)
type Turn = (Player, Move)
type State = Map Location Player
-- TODO add num black and white capture
-- turn into monad(s)?
type Game = (Dim, State, [(State, Turn)])
type Rank = Int
type Handicap = Int
