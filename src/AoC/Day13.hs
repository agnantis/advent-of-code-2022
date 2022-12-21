module AoC.Day13 where

import Control.Arrow ((&&&))
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice, eof, optional, many, runParser, some, try, (<|>))
import Text.Megaparsec.Char (eol, space, char, digitChar, string)

type Input = [(Item, Item)]
type Output = Int
type Parser = Parsec Void String

data Item = No Int
             | List [Item] deriving (Eq, Show)

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

parseLines :: String -> Input
parseLines = undefined

fstStar :: Input -> Output
fstStar = sum . fmap fst . filter (\(idx, (xs1, xs2)) -> xs1 `compare` xs2 == LT) . zip [1..]

sndStar :: Input -> Output
sndStar = undefined

---

mainDay13 :: IO ()
mainDay13 = do
  Right input <- runParser inputP ""<$> readFile "src/input/day13"
  print . (fstStar &&& sndStar) $ input

-- playground

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
