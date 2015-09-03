module Game.Go.Core where

import Data.List (sort, nub)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (isNothing, isJust, fromJust)
-- import Data.Rational

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

-- Calculate the neighbors of an intersection.
neighbors :: Dim -> Intersection -> [Intersection]
neighbors d i = concat $ map (\ x -> uncurry x (d, i)) [up, down, left, right]
  where
    up    d (r,c) = [(r-1,c) | r > 0]
    down  d (r,c) = [(r+1,c) | r < d-1]
    left  d (r,c) = [(r,c-1) | c > 0]
    right d (r,c) = [(r,c+1) | c < d-1]

-- Calculate the liberties of the intersection,
-- considering the chain it is part of, if any.
liberties :: Dim -> Position -> Player -> Intersection -> [Intersection]
liberties d pn pl i = ls
  where
    is = chain d pn pl i
    ls = is # concatMap (neighbors d)
            # sort
            # nub
            # filter (not . flip elem is)
            # filter (isEmpty pn)

-- Determine if a play would be suicide.
suicide :: Dim -> Position -> Player -> Play -> Bool
suicide _ _  _  Pass     = False
suicide d pn pl (Move i) = null $ liberties d pn' pl i
  where pn' = addPlay pl (Move i) pn

-- Calculate the intersections that form a chain with an intersection.
-- The chain is null if the player doesn't own the intersection.

chain :: Dim -> Position -> Player -> Intersection -> [Intersection]
chain d pn pl i = chain' xs d pn pl i
  where xs = maybe [] (\pl' -> [i | isSamePlayer pl pl']) (colorOf pn i)

chain' :: [Intersection]  -- accumulated chain
       -> Dim 
       -> Position
       -> Player
       -> Intersection    -- root of chain
       -> [Intersection]  -- new chain
chain' xs d pn pl i = if null ns then xs else xs'
  where
    ns = neighbors d i                              -- start with all neighbors
       # filter (not . flip elem xs)                -- discard intersections already in chain
       # map (\i -> (i, colorOf pn i))              -- associate color with intersection
       # filter (isJust . snd)                      -- identify occupied neighbors
       # filter (isSamePlayer pl . fromJust . snd)  -- identify new intersections in chain
       # map fst                                    -- extract the intersections
    xs' = nub . sort . concat $ xs : map (chain' (xs ++ ns) d pn pl) ns
 
isEmpty :: Position -> Intersection -> Bool
isEmpty pn i = isNothing $ M.lookup i pn
  
colorOf :: Position -> Intersection -> Maybe Player
colorOf pn i = M.lookup i pn
  
isSamePlayer :: Player -> Player -> Bool
isSamePlayer p1 p2 = p1 == p2

isOppositePlayer :: Player -> Player -> Bool
isOppositePlayer p1 p2 = not $ isSamePlayer p1 p2

-- Calculate the intersections captured by a play.
capture :: Dim -> Position -> Player -> Play -> [Intersection]
capture d pn pl Pass     = []
capture d pn pl (Move i) = ns
  where
    pn' = addPlay pl (Move i) pn                        -- position augmented by play
    pl' = oppositePlayer pl
    ns = neighbors d i
       # map (\i -> (i, colorOf pn i))                  -- associate color with intersection
       # filter (isJust . snd)                          -- identify occupied neighbors
       # filter (isOppositePlayer pl . fromJust . snd)  -- identify possible captures
       # map fst
       # map (\i -> (i, liberties d pn' pl' i))
       # filter (null . snd)
       # map fst
       # map (chain d pn' pl')
       # concat
       # sort
       # nub

oppositePlayer Black = White
oppositePlayer White = Black

-- Determine if the stone at an intersection is dead.
dead :: Dim -> Position -> Player -> Intersection -> Bool
dead d pn pl i = undefined

displayGame :: Game -> String
displayGame g@(d,ip, _) = display d (currentPosition g)

-- Render a position as a string
display :: Dim -> Position -> String
display d = displayBoard . board d

displayBoard :: [[Maybe Player]] -> String
displayBoard = unlines . map displayRow

displayRow :: [Maybe Player] -> String
displayRow = concatMap displayIntersection

displayIntersection :: Maybe Player -> String
displayIntersection = maybe "+" (\p -> if p == Black then "B" else "W") 

-- Enumerate the intersections of a position.
board :: Dim -> Position -> [[Maybe Player]]
board d pn = [ row d pn r | r <- [0..d-1] ]

-- Enumerate the intersections of a row.
row :: Dim -> Position -> Row -> [Maybe Player]
row d pn r = [ M.lookup (r, c) pn | c <- [0..d-1] ]

-- The starting point of a game without handicap.
initGame :: Dim -> Int -> Game
initGame d h = (d, initPosition d h, [])

-- Given the player ranks, determine the number of stones to
-- give the weaker player in the initial position.
handicap :: Rank -> Rank -> Int
handicap r1 r2 = undefined

-- Given the board dimension and the handicap, determine
-- the initial position.
initPositionOld :: Dim -> Int -> Position
initPositionOld d 0  = M.empty
initPositionOld d h  = foldr f M.empty $ handicapIntersections d h
  where f i = addPlay Black (Move i)

initPosition :: Dim -> Int -> Position
initPosition d h = foldr (addPlay Black . Move) M.empty (handicapIntersections d h)

-- Determine the intersections of initial stones by handicap.
handicapIntersections :: Dim -> Int -> [Intersection]
handicapIntersections d 0 = []
handicapIntersections d 1 = [ (2,2) ]
handicapIntersections d 2 = [ (2,2), (d-3,d-3) ]
handicapIntersections d 3 = [ (2,2), (d-3,d-3), (d-3,2) ]
handicapIntersections d 4 = [ (2,2), (d-3,d-3), (d-3,2), (2,d-3) ]
handicapIntersections d 5 = [ (2,2), (d-3,d-3), (d-3,2), (2,d-3), (d `div` 2, d `div` 2) ]
handicapIntersections d 6 = [ (2,2), (d-3,d-3), (d-3,2), (2,d-3), (d `div` 2, 2), (d `div` 2, d-3) ]
handicapIntersections _ h = error $ "Handicap " ++ show h ++ " is not implemented"

-- Place a stone for the player at the intersection.
addPlay :: Player -> Play -> Position -> Position
addPlay _  Pass     pn = pn
addPlay pl (Move i) pn = M.insert i pl pn

-- Return current position of a game.
currentPosition :: Game -> Position
currentPosition (_, ip, [])   = ip
currentPosition (_, _,  pnts) = fst $ last pnts

-- Advance a game by one turn.
addTurn :: Turn -> Game -> Game
addTurn t@(pl, p) g@(d,ip,pnts) = (d,ip,pnts ++ [(addPlay pl p (currentPosition g), t)])

-- Postfix application for ease of chaining moves.
x # f = f x

-- Apply a sequence of plays from the initial position,
-- starting with the given player and alternating through
-- the list.
sequenceMoves :: Position -> Player -> [Intersection] -> Position
sequenceMoves pn pl is = foldr nextPosition pn $ zip (cycle [pl, oppositePlayer pl]) is
  where nextPosition (pl, i) = addPlay pl (Move i)

-- TODO version of sequenceMoves that removes captures -- need to be in Game context
-- to record the number of captures

g0 = initGame 9 3
g1 = addTurn (White, Move (0, 0)) g0
showIt g = putStrLn $ displayGame g

someFunc :: IO ()
someFunc = putStrLn "someFunc"
