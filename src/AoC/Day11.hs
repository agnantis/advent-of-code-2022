{-# LANGUAGE RecordWildCards #-}
module AoC.Day11 where

import Control.Arrow ((&&&))
import Control.Applicative ((*>))
import Data.Char (toLower)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Text.Megaparsec (Parsec, choice, eof, many, runParser, some, try, (<|>), oneOf)
import Text.Megaparsec.Char (eol, space, alphaNumChar, char, digitChar, hexDigitChar, string)
import Data.Void (Void)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe
import Data.List (sort, product)
import Data.Ord (Down (..))

type Monkeys = IntMap Entry
type Monkey = Int
type WorryLvl = Int

data Entry = Entry {
  getMonkey :: Monkey,
  getStartingItems :: [WorryLvl],
  getOperation :: Operation,
  getTestDivBy :: Int,
  getOnTrue :: Int,
  getOnFalse :: Int,
  getNoOfInspections :: Int
} deriving (Show)

type Output = Int

type Input = [Entry]
type Pos = Int
type Item = Int

data Operation = Operation {
  operator :: Operator,
  operantA :: Operant,
  operantB :: Operant
} deriving (Show)

data Operant = Constant Int | Variable deriving (Show, Eq)
data Operator = Add | Sub | Multi | Div deriving (Show, Eq)

type Parser = Parsec Void String

addItems :: Entry -> [Int] -> Entry
addItems entry newItems = entry { getStartingItems = (getStartingItems entry) <> newItems }

emptyFromItems :: Entry -> (Int, Entry)
emptyFromItems entry = (length .getStartingItems $ entry, entry { getStartingItems = [] })

addItemToEntry :: Monkeys -> Pos -> Item -> Monkeys
addItemToEntry ms indx item =
  let mentry = do
        entry <- M.lookup indx ms
        let updatedEntry = addItems entry [item]
        pure $ M.insert indx updatedEntry ms
  in fromMaybe ms mentry
   

increaseInspectionsBy :: Entry -> Int -> Entry
increaseInspectionsBy entry no = entry { getNoOfInspections = getNoOfInspections entry + no }

monkeyMap :: Input -> Monkeys
monkeyMap = M.fromList . fmap (\e -> (getMonkey e, e))

runSingle :: Monkeys -> Pos -> Monkeys
runSingle mnks pos =
  let entry = mnks M.! pos
      output = runEntry entry :: [(Monkey,WorryLvl)]
      mnks' = updateMonkeys mnks output
      (itemNo, entry') = emptyFromItems entry
      entry'' =  increaseInspectionsBy entry' itemNo
  in M.insert (getMonkey entry) entry'' mnks'


updateMonkeys :: Monkeys -> [(Monkey, WorryLvl)] -> Monkeys
updateMonkeys mks [] = mks
updateMonkeys mks ((mNo, itm):xs) =
  let mks' = addItemToEntry mks mNo itm
  in updateMonkeys mks' xs

runOneRound :: Monkeys -> Monkeys
runOneRound mnks = go (M.keys mnks) mnks
 where
  go [] curMnk = curMnk
  go (x:xs) curMnk = go xs (runSingle curMnk x)

runXRounds :: Int -> Monkeys -> Monkeys
runXRounds 0 mnks = mnks
runXRounds x mnks = runXRounds (x-1) (runOneRound mnks)

getTotalScore :: Int -> Monkeys -> Int
getTotalScore n = product
                . fmap getDown
                . take n
                . sort
                . fmap Down
                . M.elems
                . M.map getNoOfInspections
---

execute :: Operation -> Int -> Int
execute Operation{..} v =
  let v1 = value operantA v
      v2 = value operantB v
  in calculate operator v1 v2

calculate :: Operator -> Int -> Int -> Int
calculate Add = (+)
calculate Sub = (-)
calculate Multi = (*)
calculate Div = (div)

value :: Operant -> Int -> Int
value (Constant x) _ = x
value Variable     x = x

testP :: Bool -> Parser Int
testP b = space *> string ("If " <> fmap toLower (show b) <> ": throw to monkey ") *> numberP

trueTestP, falseTestP :: Parser Int
trueTestP = testP True
falseTestP = testP False

conditionP :: Parser Int
conditionP = space *> string "Test: divisible by" *> numberP

operationP :: Parser Operation
operationP = do
  space *> string "Operation: new ="
  operant1 <- operantP
  operator <- operatorP
  operant2 <- operantP
  pure $ Operation operator operant1 operant2 


operantP :: Parser Operant
operantP = space *> ((Variable <$ string "old") <|> (Constant <$> numberP))

operatorP :: Parser Operator
operatorP = space *> choice [Multi <$ string "*", Add <$ string "+", Div <$ string "/", Sub <$ string "-"]

startingItemsP :: Parser [Int]
startingItemsP = do
  space *> string "Starting items:"
  no1 <- numberP
  rest <- many (char ',' *> numberP)
  pure $ no1:rest

monkeyP :: Parser Int
monkeyP = string "Monkey" *> numberP <* char ':'

numberP :: Parser Int
numberP = read <$> (space *> some digitChar)

entryP :: Parser Entry
entryP = Entry
      <$> monkeyP
      <*> startingItemsP
      <*> operationP
      <*> conditionP
      <*> trueTestP
      <*> falseTestP
      <*> pure 0

inputP:: Parser [Entry]
inputP = do
  entry <- entryP
  entries <- many (try(space *> entryP))
  space <|> (eol *> pure ()) <|> eof
  pure $ entry:entries
---

fstStar :: Input -> Output
fstStar = getTotalScore 2 . runXRounds 20 . monkeyMap

sndStar :: Input -> Output
sndStar = undefined

---


---
runEntry :: Entry -> [(Monkey, WorryLvl)]
runEntry Entry{..} = fmap mapper getStartingItems
 where
  mapper x =
    let worryLvl = (execute getOperation x) `div` 3
        monkey = if (worryLvl `mod` getTestDivBy) == 0 then getOnTrue else getOnFalse
    in (monkey, worryLvl)

mainDay11 :: IO ()
mainDay11 = do
  (Right input) <- runParser inputP "" <$> readFile "src/input/day11"
  print . (fstStar &&& sndStar) $ input

testInput :: [String]
testInput =
  [ "Monkey 0:"
  , "  Starting items: 79, 98"
  , "  Operation: new = old * 19"
  , "  Test: divisible by 23"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 3"
  , ""
  , "Monkey 0:"
  , "  Starting items: 78, 53, 89, 51, 52, 59, 58, 85"
  , "  Operation: new = old * 3"
  , "  Test: divisible by 5"
  , "    If true: throw to monkey 2"
  , "    If false: throw to monkey 7"
  ]
