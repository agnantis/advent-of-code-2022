{-

-}

module AoC.Day2 (mainDay2) where

import Control.Arrow ((&&&))

data PRS = R | P | S deriving (Show)
data Result = Win | Loss | Draw deriving (Show)

fromChar :: Char -> PRS
fromChar 'A' = R
fromChar 'X' = R
fromChar 'B' = P
fromChar 'Y' = P
fromChar 'C' = S
fromChar 'Z' = S
fromChar c   = error $ "Invalid character: " <> show c

shapeValue :: PRS -> Int
shapeValue R = 1
shapeValue P = 2
shapeValue S = 3

resultValue :: Result -> Int
resultValue Win = 6
resultValue Draw = 3
resultValue Loss = 0

result :: PRS -> PRS -> Result
result R P = Win
result R S = Loss
result P R = Loss
result P S = Win
result S P = Loss
result S R = Win
result _ _ = Draw

decode :: PRS -> Result
decode R = Loss -- X
decode P = Draw -- Y
decode S = Win  -- Z

selectShape :: PRS -> Result -> PRS
selectShape R  Win  = P
selectShape R  Loss = S
selectShape P  Loss = R
selectShape P  Win  = S
selectShape S  Loss = P
selectShape S  Win  = R
selectShape x  Draw = x


type Input = [(PRS, PRS)]
type Output = Int

parse :: String -> (PRS, PRS)
parse (a:' ':b:[]) = (fromChar a, fromChar b)
parse line = error $ "Unable to parse data: " <> line

---

fstStar :: Input -> Output
fstStar = sum . fmap (\(a,b) -> resultValue (result a b) + shapeValue b)

sndStar :: Input -> Output
sndStar = fstStar . fmap (\(a, b) -> (a, selectShape a (decode b)))

---

mainDay2 :: IO ()
mainDay2 = do
  input <- fmap parse . lines <$> readFile "src/input/day2"
  print . (fstStar &&& sndStar) $ input
