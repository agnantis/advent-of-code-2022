module AoC.Day13 where

import Control.Arrow ((&&&))
import Data.List (foldl', intersperse, sort)
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, eof, optional, many, runParser, some, try, (<|>))
import Text.Megaparsec.Char (eol, space, char, digitChar, string)

import AoC.Utils (splitWhen)

type Input = [(Item, Item)]
type Output = Int
type Parser = Parsec Void String

data Item = No Int
             | List [Item] deriving (Eq)

-- example: [[4,4],4,4], [[4,4],4,4,4]
example :: Item
example =
  let no4 = No 4
  in List [ List [no4, no4], no4, no4,List [no4, no4], no4, no4, no4]
---
-- parser
inputP :: Parser Input
inputP = some itemPairP <* eof

itemPairP :: Parser (Item, Item)
itemPairP = do
  fstItem <- optional eol *> itemP <* eol
  sndItem <- itemP <* optional eol
  pure (fstItem, sndItem)

itemP :: Parser Item
itemP = numberP <|> emptyListP <|> listP

numberP :: Parser Item
numberP = No . read <$> (some digitChar)

emptyListP :: Parser Item
emptyListP = string "[]" *> pure (List [])

listP :: Parser Item
listP = do
  _    <- char '['
  itm  <- itemP
  itms <- many (char ',' *> itemP)
  _    <- char ']'
  pure $ List (itm:itms)


-- 

instance Show Item where
  show (No i) = show i
  show (List xs) = "[" <> concat (intersperse "," (fmap show xs)) <> "]"

instance Ord Item where
  compare (No i) (No j) = compare i j
  compare (No i) lst = compare (List [No i]) lst
  compare lst (No i) = compare lst (List [No i])
  compare (List []) (List []) = EQ
  compare (List []) _ = LT
  compare _ (List []) = GT
  compare (List (x:xs)) (List (y:ys)) = case compare x y of
    EQ -> compare xs ys
    other -> other

mergeInput :: Input -> [Item]
mergeInput = foldl' (\xs (l1, l2) -> l1:l2:xs) []

divider1, divider2 :: Item
divider1 = List [List [No 2]]
divider2 = List [List [No 6]]

fstStar :: Input -> Output
fstStar = sum . fmap fst . filter (\(_, (xs1, xs2)) -> xs1 `compare` xs2 == LT) . zip [1..]

sndStar :: Input -> Output
sndStar input =
  let sortedList = sort . (<>) [divider1, divider2] . mergeInput $ input
      (p1:p2:_) = splitWhen (\ls -> ls == divider1 || ls == divider2) sortedList
      lp1 = length p1 + 1
      lp2 = length p2 + 1
  in lp1 * (lp1 + lp2)

testFun = sort . mergeInput
---

mainDay13 :: IO ()
mainDay13 = do
  Right input <- runParser inputP ""<$> readFile "src/input/day13"
  print . (fstStar &&& sndStar) $ input

-- playground

testInput :: [String]
testInput = [
  "[1,1,3,1,1]",
  "[1,1,5,1,1]",
  "",
  "[[1],[2,3,4]]",
  "[[1],4]",
  "",
  "[9]",
  "[[8,7,6]]",
  "",
  "[[4,4],4,4]",
  "[[4,4],4,4,4]",
  "",
  "[7,7,7,7]",
  "[7,7,7]",
  "",
  "[]",
  "[3]",
  "",
  "[[[]]]",
  "[[]]",
  "",
  "[1,[2,[3,[4,[5,6,7]]]],8,9]",
  "[1,[2,[3,[4,[5,6,0]]]],8,9]"
  ]
