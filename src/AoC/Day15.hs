
{-# LANGUAGE RecordWildCards #-}
module AoC.Day15 where

import Control.Arrow ((&&&))
import Text.Megaparsec (Parsec, eof, optional, runParser, some)
import Text.Megaparsec.Char (eol, digitChar, string)
import Data.Void (Void)
import Data.Maybe (fromMaybe)
import Data.List ( sort )
import Data.IntMap (IntMap)

type Input = [Entry]
type Output = Int

type Coords = (Int, Int)
data Entry = Entry {
  sensor :: Coords,
  beacon :: Coords
} deriving (Show, Eq)
type Distance = Int

type Parser = Parsec Void String

data Element = Sensor | Beacon | BeaconFree | None deriving (Show, Eq)
type Board = IntMap (IntMap Element)

targetY :: Int
targetY = 2000000
---

numberP :: Parser Int
numberP = do
  maybeNeg <- optional (string "-")
  digs <- some digitChar
  pure . read $ fromMaybe "" maybeNeg <> digs

coordP :: Parser Coords
coordP = do
  x <- string "x=" *> numberP
  y <- string ", y=" *> numberP
  pure (x,y)

-- Sensor at x=2, y=18: closest beacon is at x=-2, y=15
lineP :: Parser Entry
lineP = do
  snsr <- string "Sensor at " *> coordP
  bcn <- string ": closest beacon is at " *> coordP
  pure $ Entry snsr bcn

inputP :: Parser Input
inputP = some (lineP <* eol) <* eof

---

-- insert :: Board -> (Coords, Element) -> Board
-- insert brd ((x,y), elm) =
--   let yBucket = M.findWithDefault M.empty y brd
--       yBucket' = M.insertWith updateEntry x elm yBucket
--   in M.insert y yBucket' brd

-- fillBoard :: Input -> Board
-- fillBoard input =
--   let entries = concatMap expandEntry input
--   in foldl' insert M.empty entries

expandEntry :: Entry -> [(Int, Int)]
expandEntry Entry{..} =
  let dst = distance sensor beacon
      cArea = coverArea sensor beacon dst
  in if snd sensor == targetY then (fst sensor, fst sensor):cArea else cArea

updateEntry :: Element -> Element -> Element
-- assertion
updateEntry Beacon BeaconFree = error "It cannot be a beacon/beaconfree location"
updateEntry Sensor _ = Sensor
updateEntry Beacon _ = Beacon
updateEntry _ new = new

distance :: Coords -> Coords -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

coverArea :: Coords -> Coords -> Distance -> [(Int, Int)]
coverArea s@(x,y) b@(x', y') dist =
  let yDist = abs (y - targetY)
      xRange = dist - yDist
      (l, r) = (x-xRange, x+xRange)
      range = case () of
        _ | x' == l && x' == r -> []
          | x' == l -> [(l+1, r)]
          | x' == r -> [(l, r-1)]
          | otherwise -> []
  in  if yDist <= dist then if y' == targetY then range else [(l,r)] else []

nonBeaconPositions :: Input -> Output
nonBeaconPositions = sum . fmap (\(x, y) -> y - x + 1) . mergeIntervals . concatMap expandEntry

mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals intervals = go sorted
 where
  sorted = sort intervals
  go :: [(Int, Int)] -> [(Int, Int)]
  go [] = []
  go [x] = [x]
  go ((x1,y1):(x2,y2):xs) =
    if x2 <= y1 then go ((x1, max y1 y2):xs)
    else (x1,y1):go ((x2, y2):xs)

---

fstStar :: Input -> Output
fstStar = nonBeaconPositions

-- 286179: too low
-- 4886371: too high

sndStar :: Input -> Output
sndStar = undefined

---

mainDay15 :: IO ()
mainDay15 = do
  Right input <- runParser inputP "" <$> readFile "src/input/day15"
  -- Right input <- pure (runParser inputP "" (unlines testInput))
  print . (fstStar &&& sndStar) $ input

--- playground
testInput :: [String]
testInput = [ "Sensor at x=3, y=18: closest beacon is at x=-2, y=15"
            , "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
            , "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
            , "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
            , "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
            , "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
            , "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
            , "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
            , "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
            , "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
            , "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
            , "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
            , "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
            , "Sensor at x=20, y=1: closest beacon is at x=15, y=3"]
