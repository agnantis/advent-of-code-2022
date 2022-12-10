{-# LANGUAGE TypeApplications #-}

module AoC.Day8 where

import Control.Arrow ((&&&))
import Data.Array.IArray (Array, array, (!), bounds)
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace

type Input = Array (Int, Int) Int
type Output = Int

---

fstStar :: Input -> Output
fstStar = visibleTrees

sndStar :: Input -> Output
sndStar = undefined

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

parseLines :: String -> Input
parseLines input =
  let lns = lines input
      markRows = zip [0..] lns
      lst = concatMap(\(x, row) -> fmap (\(y, v) -> ((x,y), read @Int [v]))  $ zip [0..] row) markRows
      ar = (array ((0, 0), (length lns - 1, length (head lns) - 1)) lst) :: Input
  in ar

data Mode = RowL | RowR | ColumnU | ColumnD

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

fenceTrees :: Input -> Int
fenceTrees ar =
  let ((x0, y0), (x1, y1)) = bounds ar
  in 2*(x1-x0) + 2*(y1-y0)

type XPos = Int
type YPos = Int
type Height = Int
type Coords = (XPos, YPos)

visibleTreesLine :: [(Coords, Height)] -> Set Coords
visibleTreesLine [] = S.empty
visibleTreesLine ((coords, h):xs) = fst $ foldl' folder (S.singleton coords, h) xs
 where
  folder :: (Set Coords, Height) -> (Coords, Height) -> (Set Coords, Height)
  folder acc@(st, mx) (coords', h')
    | h' > mx = (S.insert coords' st, h')
    | otherwise = acc

-- TODO: need to keep track of the positions to avoid duplicates
visibleTrees :: Input -> Output
visibleTrees ar =
  let ((x0, y0), (x1, y1)) = bounds ar
      rowl = S.unions . fmap ( visibleTreesLine . traverse' ar RowL) $ [x0..x1]
      rowr = S.unions . fmap ( visibleTreesLine . traverse' ar RowR) $ [x0..x1]
      coll = S.unions . fmap ( visibleTreesLine . traverse' ar ColumnU) $ [y0..y1]
      colr = S.unions . fmap ( visibleTreesLine . traverse' ar ColumnD) $ [y0..y1]
      allVisible = S.unions [rowl, rowr, coll, colr]
   in length allVisible


