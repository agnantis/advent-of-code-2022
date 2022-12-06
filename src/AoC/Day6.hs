module AoC.Day6 where

import Control.Arrow ((&&&))
import Data.List (nub)

type Input = [Char]
type Output = Int


allDifferent :: Eq a => Int -> [a] -> Bool
allDifferent n = (== n) . length . nub . take n

searchForUnique :: Eq a => Int -> Int -> [a] -> Int
searchForUnique n _  [] = error $ "Unable to find " <> show n <> " successive unique elements"
searchForUnique n indx xs
  | allDifferent n xs = indx
  | otherwise         = searchForUnique n (indx+1) (tail xs)

---

fstStar :: Input -> Output
fstStar = searchForUnique 4 4

sndStar :: Input -> Output
sndStar = searchForUnique 14 14

---

mainDay6 :: IO ()
mainDay6 = do
  input <- readFile "src/input/day6"
  print . (fstStar &&& sndStar) $ input
