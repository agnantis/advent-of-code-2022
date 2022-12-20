{-# LANGUAGE RecordWildCards #-}
module AoC.Day12 (mainDay12) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Array.IArray (Array, assocs, array, bounds, (!), (//))
import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace

--- types

type HeighMap = Array Coords Int
type Output = Int
type Coords = (Int, Int)

data Input = Config {
  startPos :: Coords,
  endPos :: Coords,
  heightMap :: HeighMap
}

data State = State {
  visited :: Set Coords,
  frontier :: Set Node
} deriving Show
                  -- HeursticValue #VisitedSPos, Pos
newtype Node = Node (Int, Int, Coords) deriving (Show, Eq, Ord)

---

initState :: Input -> State
initState input@Config{..} = 
  let startNode = Node ((heuristic input 0 startPos), 0, startPos)
  in State S.empty (S.singleton startNode)

ordS, ordE :: Int
ordS = 83
ordE = 69

popMin :: Ord a => Set a -> (a, Set a)
popMin st =
  let elm = S.findMin st
  in (elm, S.delete elm st)

---

findBestPath :: Input -> State -> Int
findBestPath input@Config{..} state@State{..} =
  let noMoreMoves = null frontier
      (Node (_, v, curPos), frontier') = popMin frontier
      reachedGoal = curPos == endPos
      curHeight = heightMap ! curPos
      possibleNextMoves = nextSteps input (curPos, curHeight) state
      possibleNextNodes = fmap (\c -> Node (heuristic input v c, v+1, c)) possibleNextMoves
      frontier'' = frontier' `S.union` (S.fromList possibleNextNodes)
      visited' = S.insert curPos visited
      st' = state { visited = visited', frontier = frontier'' }
  in if noMoreMoves
      then maxBound -- error "Unable to find a path!"
      else if reachedGoal
        then v
        else findBestPath input st'


nextSteps :: Input -> (Coords, Int) -> State -> [Coords]
nextSteps input@Config{..} (coords, height) State{..} = do
  crd <- neighbors input coords
  let nHeight = heightMap ! crd
  guard $ nHeight <= height+1
  guard $ not (crd `S.member` visited)
  return crd

neighbors :: Input -> Coords -> [Coords]
neighbors Config{ .. } (x0, y0) = do
  let ((xMin, yMin), (xMax, yMax)) = bounds heightMap
  (x,y) <- zip [x0-1, x0+1] (repeat y0) <> zip (repeat x0) [y0-1, y0+1]
  guard $ x >= xMin && y >= yMin && x <= xMax && y <= yMax
  pure (x,y)

heuristic :: Input -> Int -> Coords -> Int
heuristic Config{..} visitedNo coord =
  let height = heightMap ! coord
  in (ord 'z' - height) + visitedNo + (distance coord endPos)

distance :: Coords -> Coords -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

---

parseLines :: String -> Input
parseLines input =
  let lns = lines input
      markRows = zip [0..] lns
      lst = concatMap(\(x, row) -> fmap (\(y, v) -> ((x,y), ord v))  $ zip [0..] row) markRows
      sPos = fst . head $ filter ((==) ordS . snd) lst
      ePos = fst . head $ filter ((==) ordE . snd) lst
      ar = (array ((0, 0), (length lns - 1, length (head lns) - 1)) lst)
      ar' = ar // [(sPos, ord 'a'), (ePos, ord 'z')]
  in Config sPos ePos ar'

---

fstStar :: Input -> Output
fstStar input = findBestPath input (initState input)

sndStar :: Input -> Output
sndStar input@Config{..} =
  let candidates = fmap fst . filter (\(_,v) -> v == ord 'a') . assocs $ heightMap  :: [Coords]
  in minimum . fmap (\s -> fstStar input { startPos = s }) $ candidates

---

mainDay12 :: IO ()
mainDay12 = do
  input <- parseLines <$> readFile "src/input/day12"
  print . (fstStar &&& sndStar) $ input

---
testInput :: [String]
testInput = [ "Sabqponm"
            , "abcryxxl"
            , "accszExk"
            , "acctuvwj"
            , "abdefghi"
            ]

{-
--- Day 12: Hill Climbing Algorithm ---
You try contacting the Elves using your handheld device, but the river you're following must be too low to get a
decent signal.

You ask the device for a heightmap of the surrounding area (your puzzle input). The heightmap shows the local area
from above broken into a grid; the elevation of each square of the grid is given by a single lowercase letter, where
a is the lowest elevation, b is the next-lowest, and so on up to the highest elevation, z.

Also included on the heightmap are marks for your current position (S) and the location that should get the best
signal (E). Your current position (S) has elevation a, and the location that should get the best signal (E) has
elevation z.

You'd like to reach E, but to save energy, you should do it in as few steps as possible. During each step, you can
move exactly one square up, down, left, or right. To avoid needing to get out your climbing gear, the elevation of
the destination square can be at most one higher than the elevation of your current square; that is, if your current
elevation is m, you could step to elevation n, but not to elevation o. (This also means that the elevation of the
destination square can be much lower than the elevation of your current square.)

For example:

Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
Here, you start in the top-left corner; your goal is near the middle. You could start by moving down or right, but
eventually you'll need to head toward the e at the bottom. From there, you can spiral around to the goal:

v..v<<<<
>v.vv<<^
.>vv>E^^
..v>>>^^
..>>>>>^
In the above diagram, the symbols indicate whether the path exits each square moving up (^), down (v), left (<), or
right (>). The location that should get the best signal is still E, and . marks unvisited squares.

This path reaches the goal in 31 steps, the fewest possible.

What is the fewest steps required to move from your current position to the location that should get the best signal?


--- Part Two ---
As you walk up the hill, you suspect that the Elves will want to turn this into a hiking trail. The beginning isn't
very scenic, though; perhaps you can find a better starting point.

To maximize exercise while hiking, the trail should start as low as possible: elevation a. The goal is still the
square marked E. However, the trail should still be direct, taking the fewest steps to reach its goal. So, you'll
need to find the shortest path from any square at elevation a to the square marked E.

Again consider the example from above:

Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
Now, there are six choices for starting position (five marked a, plus the square marked S that counts as being at
elevation a). If you start at the bottom-left square, you can reach the goal most quickly:

...v<<<<
...vv<<^
...v>E^^
.>v>>>^^
>^>>>>>^
This path reaches the goal in only 29 steps, the fewest possible.

What is the fewest steps required to move starting from any square with elevation a to the location that should get
the best signal?

-}
