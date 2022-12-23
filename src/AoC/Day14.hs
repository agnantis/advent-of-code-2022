{-# LANGUAGE TupleSections #-}
module AoC.Day14 where

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

withTrace :: Show a => a -> a
withTrace a = traceShow a a
--- algorithm

inBounds :: Bounds -> Coords -> Bool
inBounds ((x1, y1), (x2, y2)) (x,y) = (x1 <= x) && (x <= x2) && (y1 <= y) && (y <= y2)

elementAt :: Input -> Coords -> Element
elementAt input coords = case inBounds (bounds input) coords of
  False -> {- trace ("Not in bounds: " <> (show $ bounds input) <> " " <> (show coords)) -} TheVoid
  True  -> input ! coords

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
