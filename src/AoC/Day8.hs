{-# LANGUAGE TypeApplications #-}
module AoC.Day8 (mainDay8) where

import Control.Arrow ((&&&))
import Data.Array.IArray (Array, array, (!), bounds)
import Data.List (foldl', sort)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Ord (Down (..))

--
type Input = Array (Int, Int) Int
type Output = Int

type XPos = Int
type YPos = Int
type Height = Int
type Coords = (XPos, YPos)

data Mode = RowL | RowR | ColumnU | ColumnD deriving (Enum)

--

parseLines :: String -> Input
parseLines input =
  let lns = lines input
      markRows = zip [0..] lns
      lst = concatMap(\(x, row) -> fmap (\(y, v) -> ((x,y), read @Int [v]))  $ zip [0..] row) markRows
      ar = (array ((0, 0), (length lns - 1, length (head lns) - 1)) lst) :: Input
  in ar


traverse' :: Input -> Mode -> Int -> [(Coords, Height)]
traverse' arr mode pos = case mode of
  RowL    -> selRow
  RowR    -> reverse selRow
  ColumnU -> selCol
  ColumnD -> reverse selCol
 where
  ((x0, y0), (x1, y1)) = bounds arr
  selRow = fmap (\y -> ((pos, y), arr ! (pos, y))) [y0 .. y1]
  selCol = fmap (\x -> ((x, pos), arr ! (x, pos))) [x0 .. x1]

visibleTreesLine :: [(Coords, Height)] -> Set Coords
visibleTreesLine [] = S.empty
visibleTreesLine ((coords, h):xs) = fst $ foldl' folder (S.singleton coords, h) xs
 where
  folder :: (Set Coords, Height) -> (Coords, Height) -> (Set Coords, Height)
  folder acc@(st, mx) (coords', h')
    | h' > mx = (S.insert coords' st, h')
    | otherwise = acc

visibleTrees :: Input -> Output
visibleTrees ar =
  let ((x0, y0), (x1, y1)) = bounds ar
      rowl = S.unions . fmap ( visibleTreesLine . traverse' ar RowL) $ [x0..x1]
      rowr = S.unions . fmap ( visibleTreesLine . traverse' ar RowR) $ [x0..x1]
      coll = S.unions . fmap ( visibleTreesLine . traverse' ar ColumnU) $ [y0..y1]
      colr = S.unions . fmap ( visibleTreesLine . traverse' ar ColumnD) $ [y0..y1]
      allVisible = S.unions [rowl, rowr, coll, colr]
   in length allVisible

fromPosition :: Input -> Coords -> Int
fromPosition ar (x,y) = product . fmap go $ [RowL .. ]
 where
  ((x0, y0), (x1, y1)) = bounds ar
  curH = ar ! (x,y)
  go mode = case mode of
    RowL    -> search rfolder [y0 .. y-1]
    RowR    -> search rfolder . reverse $ [y+1 .. y1]
    ColumnU -> search cfolder [x0 .. x-1]
    ColumnD -> search cfolder . reverse $ [x+1 .. x1]
   where
     search folder = length . takeWhile id . reverse . fst . foldr folder ([], (True, curH))
     rfolder y' (vs, (previous, mx)) =
       let cr = ar ! (x, y')
       in ((previous):vs, (cr < mx, max cr mx))
     cfolder x' (vs, (previous, mx)) =
       let cr = ar ! (x', y)
       in ((previous):vs, (cr < mx , max cr mx))

findBestPlaceValue :: Input -> Output
findBestPlaceValue ar =
  let ((x0, y0), (x1, y1)) = bounds ar
      results = do
        x' <- [x0+1 .. x1-1]
        y' <- [y0+1 .. y1-1]
        pure $ fromPosition ar (x', y')
      sortedResults = sort . fmap Down $ results
  in getDown . head $ sortedResults

---
fstStar :: Input -> Output
fstStar = visibleTrees

sndStar :: Input -> Output
sndStar = findBestPlaceValue
---

mainDay8 :: IO ()
mainDay8 = do
  input <- parseLines <$> readFile "src/input/day8"
  print . (fstStar &&& sndStar) $ input

--- playground

testInput =
  [ "30373"
  , "25512"
  , "65332"
  , "33549"
  , "35390"
  ]

fenceTrees :: Input -> Int
fenceTrees ar =
  let ((x0, y0), (x1, y1)) = bounds ar
  in 2*(x1-x0) + 2*(y1-y0)

{-
--- Day 8: Treetop Tree House ---
The expedition comes across a peculiar patch of tall trees all planted carefully in a grid. The Elves explain that
a previous expedition planted these trees as a reforestation effort. Now, they're curious if this would be a good
location for a tree house.

First, determine whether there is enough tree cover here to keep a tree house hidden. To do this, you need to count
the number of trees that are visible from outside the grid when looking directly along a row or column.

The Elves have already launched a quadcopter to generate a map with the height of each tree (your puzzle input).
For example:

30373
25512
65332
33549
35390
Each tree is represented as a single digit whose value is its height, where 0 is the shortest and 9 is the tallest.

A tree is visible if all of the other trees between it and an edge of the grid are shorter than it. Only consider
trees in the same row or column; that is, only look up, down, left, or right from any given tree.

All of the trees around the edge of the grid are visible - since they are already on the edge, there are no trees
to block the view. In this example, that only leaves the interior nine trees to consider:

The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of
height 5 are in the way.)
The top-middle 5 is visible from the top and right.
The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height
0 between it and an edge.
The left-middle 5 is visible, but only from the right.
The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most
height 2 between it and an edge.
The right-middle 3 is visible from the right.
In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
With 16 trees visible on the edge and another 5 visible in the interior, a total of 21 trees are visible in this
arrangement.

Consider your map; how many trees are visible from outside the grid?

--- Part Two ---
Content with the amount of tree cover available, the Elves just need to know the best spot to build their tree
house: they would like to be able to see a lot of trees.

To measure the viewing distance from a given tree, look up, down, left, and right from that tree; stop if you reach
an edge or at the first tree that is the same height or taller than the tree under consideration. (If a tree is
right on the edge, at least one of its viewing distances will be zero.)

The Elves don't care about distant trees taller than those found by the rules above; the proposed tree house has
large eaves to keep it dry, so they wouldn't be able to see higher than the tree house anyway.

In the example above, consider the middle 5 in the second row:

30373
25512
65332
33549
35390
Looking up, its view is not blocked; it can see 1 tree (of height 3).
Looking left, its view is blocked immediately; it can see only 1 tree (of height 5, right next to it).
Looking right, its view is not blocked; it can see 2 trees.
Looking down, its view is blocked eventually; it can see 2 trees (one of height 3, then the tree of height 5 that
blocks its view).
A tree's scenic score is found by multiplying together its viewing distance in each of the four directions. For this
tree, this is 4 (found by multiplying 1 * 1 * 2 * 2).

However, you can do even better: consider the tree of height 5 in the middle of the fourth row:

30373
25512
65332
33549
35390
Looking up, its view is blocked at 2 trees (by another tree with a height of 5).
Looking left, its view is not blocked; it can see 2 trees.
Looking down, its view is also not blocked; it can see 1 tree.
Looking right, its view is blocked at 2 trees (by a massive tree of height 9).
This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house.

Consider each tree on your map. What is the highest scenic score possible for any tree?
-}
