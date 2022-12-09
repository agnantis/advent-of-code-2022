{-# LANGUAGE NumericUnderscores #-}
module AoC.Day7 where

import Control.Arrow ((&&&))
import AoC.Utils (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (sort)
import Data.Maybe
import Debug.Trace

type Input = [CMD]
data Output = Fst Int | Snd Int deriving Show

---
type Size = Int
type Name = String

data CMD = CMD_CD Name
         | CMD_LS
         | CMD_Dir Name
         | CMD_File Name Size deriving (Show, Eq)

data File = File {
  getSize :: Size
} deriving (Show, Eq)

data Directory = Directory {
  files :: Map String File,
  dirs :: Map String Directory
} deriving (Show, Eq)

filesystemSize, minUnusedSpace, maxUsedSpace :: Int
filesystemSize = 70_000_000
minUnusedSpace = 30_000_000
maxUsedSpace = filesystemSize - minUnusedSpace

ppPrintDir :: Name -> Int -> Directory -> [String]
ppPrintDir n ind d =
  let fls = map (\(k, v) -> replicate (ind+2) ' ' <> "- "<> k <> " (" <> show (getSize v) <> ")") . M.toList $ files d
      drs = concatMap (\(k, v) -> ppPrintDir k (ind+2) v) . M.toList $ dirs d
      this = replicate ind ' ' <> "- " <> n <> "(" <> show (dirSize d) <> ")"
  in this:(fls <> drs)

emptyDir :: Directory
emptyDir = Directory M.empty M.empty

fstStar :: Input -> Output
fstStar = Fst . sum . filter (<= 100000) . snd . traverse' . peelOffToRoot . snd . createTree [] emptyDir
-- fstStar = unlines . ppPrintDir. peelOffToRoot . snd . createTree [] emptyDir

sndStar :: Input -> Output
sndStar input =
  let (rt: rest) = (snd . traverse' . peelOffToRoot . snd . createTree [] emptyDir $ input) :: [Int]
      candidates = sort . filter (\x -> rt - x <= maxUsedSpace) $ rest
  in Snd $ head candidates
  -- in Snd . unlines . ppPrintDir "<root>" 0 . peelOffToRoot . snd . createTree [] emptyDir $ input
  -- in Snd . peelOffToRoot . snd . createTree [] emptyDir $ input
  -- in Snd (rt:rest)

readLine :: String -> CMD
readLine ('$':' ':'c':'d':' ':xs) = CMD_CD xs
readLine ('$':' ':'l':'s':_) = CMD_LS
readLine ('d':'i':'r':' ':xs) = CMD_Dir xs
readLine xs =
  let [size, name] = splitOn ' ' xs
  in CMD_File name (read size)

peelOffToRoot :: Directory -> Directory
peelOffToRoot = fromJust . M.lookup "/" . dirs

createTree :: [Directory] -> Directory -> [CMD] -> ([CMD], Directory)
createTree prs dr [] = ([], dr)
createTree prs dr (CMD_LS:xs) = createTree prs dr xs -- no op
createTree prs dr (CMD_File n s:xs) = createTree prs (addFile dr n s) xs
createTree prs dr (CMD_Dir n:xs) = createTree prs (addDir dr n emptyDir) xs
createTree prs dr (CMD_CD ".":xs) = createTree prs dr xs
createTree (r:rs) dr (CMD_CD "..":xs) = (xs, dr) -- createTree rs r xs
createTree prs dr (CMD_CD n:xs) =
    let childDir = M.findWithDefault emptyDir n $ dirs dr
        (cmds, childDir') = createTree (dr:prs) childDir xs
    in createTree prs (addDir dr n childDir') cmds

addFile :: Directory -> Name -> Size -> Directory
addFile dr n s =
  let mp  = files dr
      mp' = M.insert n (File s) mp
  in dr { files = mp' }

addDir :: Directory -> Name -> Directory -> Directory
addDir parent n child =
  let mp  = dirs parent
      mp' = M.insert n child mp
  in parent { dirs = mp' }

dirSize :: Directory -> Size
dirSize dr =
  let fSize = sum . map getSize . M.elems . files $ dr
      dSize = sum . map dirSize . M.elems . dirs $ dr
  in fSize + dSize

-- TODO: Should return its size and their children in a separate list
traverse' :: Directory -> (Int, [Int])
traverse' dr =
  let childDirs = fmap traverse' . M.elems . dirs $ dr
      dSizes = sum . fmap fst $ childDirs
      children = concatMap snd childDirs
      fSizes = sum . map getSize . M.elems . files $ dr
      totalSize = fSizes + dSizes
  in ((totalSize), totalSize:children)

traverse1 :: Name -> Directory -> [(String, Int)]
traverse1 nm dr =
  let children = concatMap (\(n,s) -> traverse1 n s) . M.toList . dirs $ dr
      fsizes = sum . map getSize . M.elems . files $ dr
      childrenSize = sum . fmap snd $ children
  -- in trace ("d: " <> show childrenSize <> " f: " <> show fsizes) $ (fsizes + childrenSize):children
  in ((nm, fsizes + childrenSize)):children

---

mainDay7' :: IO Output
mainDay7' = do
  input <- fmap readLine . lines <$> readFile "src/input/day7"
  return $ sndStar input

mainDay7 :: IO ()
mainDay7 = do
  input <- fmap readLine . lines <$> readFile "src/input/day7"
  print . (fstStar &&& sndStar) $ input

testDir :: Directory
testDir = peelOffToRoot . snd . createTree [] emptyDir $ fmap readLine cmd

cmd = [
      "$ cd /"
    , "$ ls"
    -- , "dir gts"
    -- , "68377 jvdqjhr.jvp"
    -- , "dir lwhbw"
    -- , "228884 nqth.gcn"
    , "dir pcqjnl"
    -- , "94844 ppwv.zsh"
    -- , "97889 rqpw"
    -- , "dir sqhw"
    -- , "dir vllgn"
    -- , "dir wdtm"
    -- , "dir ztfdwp"
    -- , "$ cd gts"
    -- , "$ ls"
    -- , "846 grwwbrgz.wft"
    -- , "72000 mrnhn.psz"
    -- , "155241 qvnbd.dqs"
    -- , "6655 tndtmwfv"
    -- , "$ cd .."
    -- , "$ cd lwhbw"
    -- , "$ ls"
    -- , "99946 lrrl.lth"
    -- , "$ cd .."
    , "$ cd pcqjnl"
    , "$ ls"
    -- , "76420 gdg.lvr"
    , "dir gljcvm"
    -- , "161390 hlnrq.mjj"
    -- , "dir lqwntmdg"
    -- , "dir lrrl"
    -- , "dir qgpr"
    -- , "222006 tndtmwfv"
    , "$ cd gljcvm"
    , "$ ls"
    , "264381 tmwzlzn"
    -- , "$ cd .."
    -- , "$ cd lqwntmdg"
    ]
