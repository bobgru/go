module Game.Go.Core where

import Data.List (sort, nub)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe (isNothing, isJust, fromJust)
import Game.Go.Types

-- Calculate the neighbors of a location.
neighbors :: Dim -> Location -> [Location]
neighbors d l = concat $ map (\ x -> uncurry x (d, l)) [up, down, left, right]
  where
    up    d (r,c) = [(r-1,c) | r > 0]
    down  d (r,c) = [(r+1,c) | r < d-1]
    left  d (r,c) = [(r,c-1) | c > 0]
    right d (r,c) = [(r,c+1) | c < d-1]

-- Calculate the liberties of the location,
-- considering the chain it is part of, if any.
liberties :: Dim -> State -> Player -> Location -> [Location]
liberties d s p l = ls'
  where
    ls  = chain d s p l
    ls' = ls # concatMap (neighbors d)
             # sort
             # nub
             # filter (not . flip elem ls)
             # filter (isEmpty s)

-- Determine if a play would be suicide.
suicide :: Dim -> State -> Player -> Move -> Bool
suicide _ _ _ Pass     = False
suicide d s p (Move l) = null $ liberties d s' p l
  where s' = addMove p (Move l) s

-- Calculate the locations that form a chain with a location.
-- The chain is null if the player doesn't own the location.

chain :: Dim -> State -> Player -> Location -> [Location]
chain d s p l = chain' xs d s p l
  where xs = maybe [] (\p' -> [l | isSamePlayer p p']) (ownerOf s l)

chain' :: [Location]  -- accumulated chain
       -> Dim 
       -> State
       -> Player
       -> Location    -- root of chain
       -> [Location]  -- new chain
chain' xs d pn pl i = if null ns then xs else xs'
  where
    ns = neighbors d i                              -- start with all neighbors
       # filter (not . flip elem xs)                -- discard locations already in chain
       # map (\i -> (i, ownerOf pn i))              -- associate color with location
       # filter (isJust . snd)                      -- identify occupied neighbors
       # filter (isSamePlayer pl . fromJust . snd)  -- identify new locations in chain
       # map fst                                    -- extract the locations
    xs' = nub . sort . concat $ xs : map (chain' (xs ++ ns) d pn pl) ns
 
isEmpty :: State -> Location -> Bool
isEmpty s l = isNothing $ M.lookup l s
  
ownerOf :: State -> Location -> Maybe Player
ownerOf s l = M.lookup l s
  
isSamePlayer :: Player -> Player -> Bool
isSamePlayer = (==)

isOppositePlayer :: Player -> Player -> Bool
isOppositePlayer = (/=)

-- Calculate the locations captured by a move.
capture :: Dim -> State -> Player -> Move -> [Location]
capture d s p Pass     = []
capture d s p (Move l) = ns
  where
    s' = addMove p (Move l) s                           -- state augmented by move
    p' = oppositePlayer p
    ns = neighbors d l
       # map (\l -> (l, ownerOf s l))                   -- associate color with location
       # filter (isJust . snd)                          -- identify occupied neighbors
       # filter (isOppositePlayer p . fromJust . snd)   -- identify possible captures
       # map fst
       # map (\l -> (l, liberties d s' p' l))
       # filter (null . snd)
       # map fst
       # map (chain d s' p')
       # concat
       # sort
       # nub

oppositePlayer Black = White
oppositePlayer White = Black

-- Determine if the stone at a location is dead.
dead :: Dim -> State -> Player -> Location -> Bool
dead d s p l = undefined

displayGame :: Game -> String
displayGame g@(d,_, _) = display d (currentState g)

-- Render a state as a string
display :: Dim -> State -> String
display d = displayBoard . board d

displayBoard :: [[Maybe Player]] -> String
displayBoard = unlines . map displayRow

displayRow :: [Maybe Player] -> String
displayRow = concatMap displayLocation

displayLocation :: Maybe Player -> String
displayLocation = maybe "+" (\p -> if p == Black then "B" else "W") 

-- Enumerate the locations of a state.
board :: Dim -> State -> [[Maybe Player]]
board d s = [ row d s r | r <- [0..d-1] ]

-- Enumerate the locations of a row.
row :: Dim -> State -> Row -> [Maybe Player]
row d s r = [ M.lookup (r, c) s | c <- [0..d-1] ]

-- The starting point of a game without handicap.
initGame :: Dim -> Handicap -> Game
initGame d h = (d, initState d h, [])

-- Given the player ranks, determine the number of stones to
-- give the weaker player in the initial state.
handicap :: Rank -> Rank -> Handicap
handicap r1 r2 = undefined

-- Given the board dimension and the handicap, determine
-- the initial state.
initStateOld :: Dim -> Handicap -> State
initStateOld d 0  = M.empty
initStateOld d h  = foldr f M.empty $ handicapLocations d h
  where f i = addMove Black (Move i)

initState :: Dim -> Handicap -> State
initState d h = foldr (addMove Black . Move) M.empty (handicapLocations d h)

-- Determine the locations of initial stones by handicap.
handicapLocations :: Dim -> Handicap -> [Location]
handicapLocations d 0 = []
handicapLocations d 1 = [ (2,2) ]
handicapLocations d 2 = [ (2,2), (d-3,d-3) ]
handicapLocations d 3 = [ (2,2), (d-3,d-3), (d-3,2) ]
handicapLocations d 4 = [ (2,2), (d-3,d-3), (d-3,2), (2,d-3) ]
handicapLocations d 5 = [ (2,2), (d-3,d-3), (d-3,2), (2,d-3), (d `div` 2, d `div` 2) ]
handicapLocations d 6 = [ (2,2), (d-3,d-3), (d-3,2), (2,d-3), (d `div` 2, 2), (d `div` 2, d-3) ]
handicapLocations _ h = error $ "Handicap " ++ show h ++ " is not implemented"

-- Place a stone for the player at the location.
addMove :: Player -> Move -> State -> State
addMove _  Pass     s = s
addMove p  (Move i) s = M.insert i p s

-- Return current state of a game.
currentState :: Game -> State
currentState (_, s, []) = s
currentState (_, _, ss) = fst $ last ss 

-- Advance a game by one turn.
addTurn :: Turn -> Game -> Game
addTurn t@(p, m) g@(d,s,ss) = (d,s,ss ++ [(addMove p m (currentState g), t)])

-- Postfix application for ease of chaining moves.
x # f = f x

-- Apply a sequence of plays from the initial state,
-- starting with the given player and alternating through
-- the list.
sequenceMoves :: State -> Player -> [Location] -> State
sequenceMoves s p ls = foldr nextState s $ zip (cycle [p, oppositePlayer p]) ls
  where nextState (p, l) = addMove p (Move l)

sequenceTurns :: Game -> Player -> [Location] -> Game
sequenceTurns g p ls = foldr nextTurn g $ zip (cycle [p, oppositePlayer p]) ls
  where nextTurn (p, l) = addTurn (p, (Move l))

-- TODO version of sequenceMoves that removes captures -- need to be in Game context
-- to record the number of captures

g0 = initGame 9 3
g1 = addTurn (White, Move (0, 0)) g0
showIt g = putStrLn $ displayGame g

someFunc :: IO ()
someFunc = putStrLn "someFunc"
