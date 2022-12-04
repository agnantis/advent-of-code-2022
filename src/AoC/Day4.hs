module AoC.Day4  where

import Control.Arrow ((&&&))

import AoC.Utils (splitWhen)

type Section = (Int, Int)
type Input = [(Section, Section)]
type Output = Int

---

fstStar :: Input -> Output
fstStar = length . filter includes

sndStar :: Input -> Output
sndStar = undefined

-- xx-xx,xx-xx
parse :: String -> (Section, Section)
parse l =
  let [fs, fe, ss, se] = fmap read $ splitWhen (\c -> c `elem` ",-") l
  in ((fs, fe), (ss, se))

includes :: (Section, Section) -> Bool
includes ((fs, fe), (ss, se))
  | fs <= ss && fe >= se = True
  | ss <= fs && se >= fe = True
  | otherwise            = False
---

mainDay4 :: IO ()
mainDay4 = do
  input <- fmap parse . lines <$> readFile "src/input/day4"
  print . (fstStar &&& sndStar) $ input
