module AoC.Day1 (mainDay) where

import Control.Arrow ((&&&))
import Data.List (sort)
import Data.Ord (Down (..), getDown)
import AoC.Utils (splitWhen)

type Input = [[Int]]
type Output = Int

fstStar :: Input -> Output
fstStar = maximum . fmap sum

sndStar :: Input -> Output
sndStar = getDown . sum . take 3 . sort . fmap (Down . sum)

mainDay :: IO  ()
mainDay = do
  strInput <- splitWhen (\x -> x == "") . lines <$> readFile "src/input/day1"
  let input = (fmap . fmap) read $ strInput
  print . (fstStar &&& sndStar) $ input


{-
 -
 -}
