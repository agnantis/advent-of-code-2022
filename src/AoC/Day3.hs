module AoC.Day3 (mainDay3, createMap) where

import Control.Arrow ((&&&))
import Data.Char (ord, isLower)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Input = [String]
type Output = Int

---

fstStar :: Input -> Output
fstStar = sum . fmap (charValue . duplicate . createMap)

sndStar :: Input -> Output
sndStar = sum. fmap (charValue . findCommon) . groupsOf3


createMap :: String -> (Int, Map Char [Int])
createMap line =
  let group = zip line (fmap pure [1..]) -- [(c, [index])...
      middle = length line `div` 2
  in (middle, M.fromListWith (<>) group)

duplicate :: (Int, Map Char [Int]) -> Char
duplicate (m, mp) =
  let ls = M.toList mp
      Just (c, _) = find (\(_, idx) -> any (<= m) idx && any (> m) idx) ls
  in c

charValue :: Char -> Int
charValue c =
  let padding = if isLower c then 96 else 38
  in ord c - padding

findCommon :: (String, String, String) -> Char
findCommon (a, b, c) =
  let Just r = find (\ch -> ch `elem` b && ch `elem` c) a
  in r

groupsOf3 :: [a] -> [(a, a, a)]
groupsOf3 (a:b:c:xs) = (a,b,c):(groupsOf3 xs)
groupsOf3 _          = []
---

mainDay3 :: IO ()
mainDay3 = do
  input <- lines <$> readFile "src/input/day3"
  print . (fstStar &&& sndStar) $ input


