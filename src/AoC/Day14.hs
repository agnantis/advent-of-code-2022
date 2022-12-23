{-# LANGUAGE TupleSections #-}
module AoC.Day14 (mainDay14) where

import Control.Arrow ((&&&))
import Data.Array.IArray (Array, array, accumArray, (!), (//), bounds, assocs)
import Data.Ix (Ix)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, optional, many, runParser, some, (<|>))
import Text.Megaparsec.Char (eol, char, digitChar, string)

import AoC.Utils
import Debug.Trace

type Coords = (Int, Int)
type Input = Array Coords Element
type RawData = [[Coords]]
type Output = Int

type Bounds = (Coords, Coords)
type Parser = Parsec Void String

data Element = Air | Rock | Sand | TheVoid deriving (Eq)

instance Show Element where
  show Air = "."
  show Rock = "#"
  show Sand = "o"
  show TheVoid = "~"

entryPoint :: Coords
entryPoint = (500,0)

numberP :: Parser Int
numberP = read <$> (some digitChar)

pairP :: Parser (Int, Int)
pairP = do
  x <- numberP <* char ','
  y <- numberP
  pure (x,y)

lineP :: Parser [(Int, Int)]
lineP = do
  xy <- pairP
  xys <- many (string " -> " *> pairP)
  pure (xy:xys)

inputP :: Parser RawData
inputP = some (lineP <* eol)
---

calcBounds :: RawData -> Bounds
calcBounds input =
  let input' = entryPoint:(concat input)
      xMin = minimum . fmap fst $ input'
      xMax = maximum . fmap fst $ input'
      yMin = minimum . fmap snd $ input'
      yMax = maximum . fmap snd $ input'
  in ((xMin, yMin), (xMax, yMax))

populateArray :: Bounds -> RawData -> Input
populateArray b i =
  let expanded = concatMap expandValues i
  in accumArray (flip const) Air b $ zip expanded (repeat Rock)

expandValues :: [(Int, Int)] -> [(Int, Int)]
expandValues [] = []
expandValues [x] = [x]
expandValues xs@((x1, y1):(x2, y2):_) =
  let expd = if x1 == x2 
              then expandVertically x1 y1 y2
              else expandHorizontally x1 x2 y1
  in expd <> (expandValues . tail $ xs)

expandHorizontally :: Int -> Int -> Int -> [(Int, Int)]
expandHorizontally x1 x2 y =
  let xs = if x1 < x2 then [x1 .. x2] else [x2 .. x1]
  in fmap (,y) xs

expandVertically :: Int -> Int -> Int -> [(Int, Int)]
expandVertically x y1 y2 =
  let ys = if y1 < y2 then [y1 .. y2] else [y2 .. y1]
  in fmap (x,) ys

createInput :: RawData -> Input
createInput rawData = populateArray (calcBounds rawData) rawData

--- algorithm

inBounds :: Bounds -> Coords -> Bool
inBounds ((x1, y1), (x2, y2)) (x,y) = (x1 <= x) && (x <= x2) && (y1 <= y) && (y <= y2)

elementAt :: Input -> Coords -> Element
elementAt input coords = case inBounds (bounds input) coords of
  False -> TheVoid
  True  -> input ! coords

-- If position found -> Right position
-- If position is void -> Left position
-- If entry is blocked -> Left Nothing
dropSand :: Input -> Either (Maybe Coords) Coords
dropSand input = go entryPoint
 where
  go xy@(x,y) =
    let curIsBlocked = (elementAt input xy) /= Air
        downPos = (x, y+1)
        downLeftPos = (x-1, y+1)
        downRightPos = (x+1, y+1)
        downElem = elementAt input downPos
        downLeftElem = elementAt input downLeftPos
        downRightElem = elementAt input downRightPos
    in case curIsBlocked of
        True -> Left Nothing
        False -> case downElem of
          TheVoid -> Left . Just $ downPos
          Air     -> go downPos
          _       -> case downLeftElem of
            TheVoid -> Left . Just $ downLeftPos
            Air     -> go downLeftPos
            _       -> case downRightElem of
              TheVoid -> Left . Just $ downRightPos
              Air     -> go downRightPos
              _       -> Right (x,y)

keepDropping :: Input -> Int
keepDropping = go 0
 where
  -- uncomment for a cute animation :)
  go no input = case {- (trace . unlines . printInput $ input) -} dropSand input of
                Left _ -> no
                Right coords -> go (no+1) (input // [(coords, Sand)])

keepDropping2 :: Input -> Int
keepDropping2 = go 0
 where
  -- uncomment for a cute animation :)
  go no input = case {- (trace . unlines . printInput $ input) -} dropSand input of
                  Left Nothing      -> no
                  Left (Just (x,_)) -> go no (addColumn x input)
                  Right coords      -> go (no+1) (input // [(coords, Sand)])

withFloor :: Input -> Input
withFloor input =
  let ((x0, y0), (x1, y1)) = bounds input
      aboveFloor = zip (zip [x0 .. x1] (repeat $ y1+1)) (repeat Air)
      floorRocks = zip (zip [x0 .. x1] (repeat $ y1+2)) (repeat Rock)
      newBounds = ((x0, y0), (x1, y1+2))
  in expandArray newBounds input (aboveFloor <> floorRocks) 

expandArray :: Ix i => (i, i) -> Array i a -> [(i, a)] -> Array i a
expandArray bnds arr newItems = array bnds (assocs arr <> newItems)

addColumn :: Int -> Input -> Input
addColumn x input =
  let ((x0, y0), (x1, y1)) = bounds input
      airColumn = ((x,y1), Rock):(zip (zip (repeat $ x) [y0 .. y1-1]) (repeat Air)) :: [(Coords, Element)]
      newBounds = ((min x0 x, y0), (max x x1, y1))
  in {- trace ("Adding column " <> show x) $ -} expandArray newBounds input airColumn


fstStar :: Input -> Output
fstStar = keepDropping

sndStar :: Input -> Output
sndStar = keepDropping2 . withFloor

---

mainDay14 :: IO ()
mainDay14 = do
  Right input <-  runParser inputP "" <$> readFile "src/input/day14"
  print . (fstStar &&& sndStar) . createInput $ input

-- playground
printInput :: Input -> [String]
printInput input =
  let ((x0, y0), (x1, y1)) = bounds input
  in fmap (\y -> concatMap (\x -> show (input ! (x,y))) [x0..x1]) [y0..y1]

testInput :: [String]
testInput = [
  "498,4 -> 498,6 -> 496,6",
  "503,4 -> 502,4 -> 502,9 -> 494,9"
  ]

{-
--- Day 14: Regolith Reservoir ---
The distress signal leads you to a giant waterfall! Actually, hang on - the signal seems like it's coming from the
waterfall itself, and that doesn't make any sense. However, you do notice a little path that leads behind the
waterfall.

Correction: the distress signal leads you behind a giant waterfall! There seems to be a large cave system here,
and the signal definitely leads further inside.

As you begin to make your way deeper underground, you feel the ground rumble for a moment. Sand begins pouring
into the cave! If you don't quickly figure out where the sand is going, you could quickly become trapped!

Fortunately, your familiarity with analyzing the path of falling material will come in handy here. You scan a
two-dimensional vertical slice of the cave above you (your puzzle input) and discover that it is mostly air with
structures made of rock.

Your scan traces the path of each solid rock structure and reports the x,y coordinates that form the shape of the
path, where x represents distance to the right and y represents distance down. Each path appears as a single line
of text in your scan. After the first point of each path, each point indicates the end of a straight horizontal
or vertical line to be drawn from the previous point. For example:

498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
This scan means that there are two paths of rock; the first path consists of two straight lines, and the second
path consists of three straight lines. (Specifically, the first path consists of a line of rock from 498,4 through
498,6 and another line of rock from 498,6 through 496,6.)

The sand is pouring into the cave from point 500,0.

Drawing rock as #, air as ., and the source of the sand as +, this becomes:


  4     5  5
  9     0  0
  4     0  3
0 ......+...
1 ..........
2 ..........
3 ..........
4 ....#...##
5 ....#...#.
6 ..###...#.
7 ........#.
8 ........#.
9 #########.
Sand is produced one unit at a time, and the next unit of sand is not produced until the previous unit of sand
comes to rest. A unit of sand is large enough to fill one tile of air in your scan.

A unit of sand always falls down one step if possible. If the tile immediately below is blocked (by rock or sand),
the unit of sand attempts to instead move diagonally one step down and to the left. If that tile is blocked, the
unit of sand attempts to instead move diagonally one step down and to the right. Sand keeps moving as long as it is
able to do so, at each step trying to move down, then down-left, then down-right. If all three possibl
e destinations are blocked, the unit of sand comes to rest and no longer moves, at which point the next unit of
sand is created back at the source.

So, drawing sand that has come to rest as o, the first unit of sand simply falls straight down and then stops:

......+...
..........
..........
..........
....#...##
....#...#.
..###...#.
........#.
......o.#.
#########.
The second unit of sand then falls straight down, lands on the first one, and then comes to rest to its left:

......+...
..........
..........
..........
....#...##
....#...#.
..###...#.
........#.
.....oo.#.
#########.
After a total of five units of sand have come to rest, they form this pattern:

......+...
..........
..........
..........
....#...##
....#...#.
..###...#.
......o.#.
....oooo#.
#########.
After a total of 22 units of sand:

......+...
..........
......o...
.....ooo..
....#ooo##
....#ooo#.
..###ooo#.
....oooo#.
...ooooo#.
#########.
Finally, only two more units of sand can possibly come to rest:

......+...
..........
......o...
.....ooo..
....#ooo##
...o#ooo#.
..###ooo#.
....oooo#.
.o.ooooo#.
#########.
Once all 24 units of sand shown above have come to rest, all further sand flows out the bottom, falling into the
endless void. Just for fun, the path any new sand takes before falling forever is shown here with ~:

.......+...
.......~...
......~o...
.....~ooo..
....~#ooo##
...~o#ooo#.
..~###ooo#.
..~..oooo#.
.~o.ooooo#.
~#########.
~..........
~..........
~..........
Using your scan, simulate the falling sand. How many units of sand come to rest before sand starts flowing into
the abyss below?

Your puzzle answer was 1513.

--- Part Two ---
You realize you misread the scan. There isn't an endless void at the bottom of the scan - there's floor, and you're
standing on it!

You don't have time to scan the floor, so assume the floor is an infinite horizontal line with a y coordinate equal
to two plus the highest y coordinate of any point in your scan.

In the example above, the highest y coordinate of any point is 9, and so the floor is at y=11. (This is as if your
scan contained one extra rock path like -infinity,11 -> infinity,11.) With the added floor, the example above now
looks like this:

        ...........+........
        ....................
        ....................
        ....................
        .........#...##.....
        .........#...#......
        .......###...#......
        .............#......
        .............#......
        .....#########......
        ....................
<-- etc #################### etc -->
To find somewhere safe to stand, you'll need to simulate falling sand until a unit of sand comes to rest at 500,0,
blocking the source entirely and stopping the flow of sand into the cave. In the example above, the situation
finally looks like this after 93 units of sand come to rest:

............o............
...........ooo...........
..........ooooo..........
.........ooooooo.........
........oo#ooo##o........
.......ooo#ooo#ooo.......
......oo###ooo#oooo......
.....oooo.oooo#ooooo.....
....oooooooooo#oooooo....
...ooo#########ooooooo...
..ooooo.......ooooooooo..
#########################
Using your scan, simulate the falling sand until the source of the sand becomes blocked. How many units of sand
come to rest?

-}
