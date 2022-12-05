module AoC.Day5 where

import Control.Arrow ((&&&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (foldl', partition)
import Data.Maybe (fromMaybe)

import AoC.Utils (chunksOf, splitOn, splitWhen)

type State = IntMap [Char]
type Move = (Int, Int, Int)
type Input = (State, [Move])
type Output = [Char]

---
testState :: State
testState = M.fromList [(1, "nz"), (2, "dcm"), (3, "p")]

testMoves :: [Move]
testMoves = [(1,2,1), (3,1,3), (2,2,1), (1,1,2)]

fstStar :: Input -> Output
fstStar (st, moves) = fmap head . M.elems . foldl' (move) st $ moves

sndStar :: Input -> Output
sndStar = undefined

move :: State -> Move -> State
move st (n, f, t) =
  let fPile = fromMaybe [] $ M.lookup f st
      (s1, sw) = splitAt n fPile
      st' = M.insertWith (<>) t (reverse s1) st
  in M.insert f sw st'

lns = [ "        [J]         [B]     [T]    "
      , "        [M] [L]     [Q] [L] [R]    "
      , "        [G] [Q]     [W] [S] [B] [L]"
      , "[D]     [D] [T]     [M] [G] [V] [P]"
      , "[T]     [N] [N] [N] [D] [J] [G] [N]"
      , "[W] [H] [H] [S] [C] [N] [R] [W] [D]"
      , "[N] [P] [P] [W] [H] [H] [B] [N] [G]"
      , "[L] [C] [W] [C] [P] [T] [M] [Z] [W]"
      , " 1   2   3   4   5   6   7   8   9 "
      ]

parseState :: [String] -> State
parseState = M.unionsWith (<>) . fmap parseRow

parseRow :: String -> State
parseRow = M.fromList . zip [1..] . fmap makeList . chunksOf 4
  where
    makeList ('[':x:']':_) = [x]
    makeList _             = []

parseMove :: String -> Move
parseMove line =
  let (_:s:_:f:_:t:_) = splitOn ' ' line
  in (read s, read f, read t)

parseMoves :: [String] -> [Move]
parseMoves = fmap parseMove

parseInput :: [String] -> Input
parseInput input =
 let [stateStr, movesStr] = splitWhen (== "") input
 in (parseState stateStr, parseMoves movesStr)

---

mainDay5 :: IO ()
mainDay5 = do
  input <- lines <$> readFile "src/input/day5"
  print . (fstStar &&& sndStar) . parseInput $ input
