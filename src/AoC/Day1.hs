module AoC.Day1 (mainDay) where

import AoC.Utils (splitWhen)

type Input = [[Int]]
type Output = Int

fstStar :: Input -> Output
fstStar = maximum . fmap sum

sndStar :: Input -> Output
sndStar xs = undefined

mainDay :: IO  ()
mainDay = do
  strInput <- splitWhen (\x -> x == "") . lines <$> readFile "src/input/day1"
  let input = (fmap . fmap) read $ strInput
  print . (fstStar &&& sndStar) $ input


{-
 -
 -}
