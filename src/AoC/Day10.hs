{-# LANGUAGE BangPatterns #-}
module AoC.Day10 where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Debug.Trace

type Input = [Command]
type Output = Int

data Command = Add Int
             | Noop
             | Interm deriving (Show, Eq)

stops = [20, 40, 40, 40, 40, 40]
initialValue = 1

parseLine :: String -> [Command]
parseLine ('a':'d':'d':'x':' ':no) = [Interm, Add (read no)]
parseLine _ = [Noop]

createMask :: [Int] -> [Bool]
createMask [] = []
createMask (x:xs) = ((replicate (x-1) False) <> [True]) <> (createMask xs)

traverse' :: Input -> Int
traverse' input =
  let zipped = zip3 (createMask stops) [1..] input
  in snd . foldl' folder (initialValue, 0) $ zipped
 where
  folder :: (Int, Int) -> (Bool, Int, Command) -> (Int, Int)
  folder a@(xV, !acc) i@(msk, indx, Add v)  =
    let newX = xV+v
    in trace ("Acc: " <> show a <> "\tinput: " <> show i) $ if msk then (newX, acc+(indx*(xV))) else (newX, acc)
  folder a@(xV, !acc) i@(msk, indx, _)  =
    trace ("Acc: " <> show a <> "\tinput: " <> show i) $ if msk then (xV, acc+(indx*xV)) else (xV, acc)
---
-- wrong : 14320
-- wrong : 14780

fstStar :: Input -> Output
fstStar = traverse'

sndStar :: Input -> Output
sndStar = undefined

---

mainDay10 :: IO ()
mainDay10 = do
  input <- concatMap parseLine . lines <$> readFile "src/input/day10"
  print . (fstStar &&& sndStar) $ input
