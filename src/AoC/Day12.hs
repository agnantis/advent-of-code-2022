{-# LANGUAGE RecordWildCards #-}
module AoC.Day12 where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Array.IArray (Array, assocs, array, bounds, (!), (//))
import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace

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

-- instance Ord Node where
--   compare (Node (h1, v1, c1)) (Node (h2, v, c2)) =
--     let hc = h1 `compare` h2
--         vc = v1 `compare` v2
--         cc = c1 `compare` c2
--     in if hc == EQ

initState :: Input -> State
initState input@Config{..} = 
  let startNode = Node ((heuristic input 0 startPos), 0, startPos)
  in State S.empty (S.singleton startNode)

ordS, ordE :: Int
ordS = 83
ordE = 69
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
      -- visited' = visited `S.union` (S.fromList possibleNextMoves)
      visited' = S.insert curPos visited
      st' = state { visited = visited', frontier = frontier'' }
  in if noMoreMoves
      then maxBound -- error "Unable to find a path!"
      else if reachedGoal
        then v -- length visited
        else {- trace (show curPos <> " State: " <> show st' ) $ -} findBestPath input st'


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
  -- in 8*(ord 'z' - height) {- + visitedNo -} + (distance coord endPos)

distance :: Coords -> Coords -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
---

fstStar :: Input -> Output
fstStar input = findBestPath input (initState input)

sndStar :: Input -> Output
sndStar input@Config{..} =
  let candidates = fmap fst . filter (\(_,v) -> v == ord 'a') . assocs $ heightMap  :: [Coords]
  in trace ("Candidates: " <> show (length candidates)) $ minimum . fmap (\s -> fstStar input { startPos = s }) $ candidates

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
