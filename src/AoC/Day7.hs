module AoC.Day7 where

import Control.Arrow ((&&&))
import AoC.Utils (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe

type Input = [CMD]
type Output = Int

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
  parentDir :: Maybe Directory,
  files :: Map String File,
  dirs :: Map String Directory
} deriving (Show, Eq)

ppPrintDir :: Name -> Int -> Directory -> [String]
ppPrintDir n ind d =
  let fls = map (\(k, v) -> replicate (ind+2) ' ' <> "- "<> k <> " (" <> show (getSize v) <> ")") . M.toList $ files d
      drs = concatMap (\(k, v) -> ppPrintDir k (ind+2) v) . M.toList $ dirs d
      this = replicate ind ' ' <> "- " <> n
  in this:(fls <> drs)

emptyDir :: Directory
emptyDir = Directory Nothing M.empty M.empty

emptyDirWithParent :: Directory -> Directory
emptyDirWithParent dr = Directory (Just dr) M.empty M.empty

fstStar :: Input -> Output
fstStar = sum . filter (<= 100000) . traverse' . peelOffToRoot . snd . createTree [] emptyDir

sndStar :: Input -> Output
sndStar = undefined

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
createTree prs dr (CMD_Dir n:xs) = createTree prs (addDir dr n (emptyDirWithParent dr)) xs
createTree prs dr (CMD_CD ".":xs) = createTree prs dr xs
createTree (r:rs) dr (CMD_CD "..":xs) = (xs, dr) -- createTree rs r xs
createTree prs dr (CMD_CD n:xs) =
    let childDir = M.findWithDefault (emptyDirWithParent dr) n $ dirs dr
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

traverse' :: Directory -> [Int]
traverse' dr =
  let children = concatMap traverse' . M.elems . dirs $ dr
      fsizes = sum . map getSize . M.elems . files $ dr
      childrenSize = sum children
  in (fsizes + childrenSize):children

---

mainDay7 :: IO ()
mainDay7 = do
  input <- fmap readLine . lines <$> readFile "src/input/day7"
  print . (fstStar &&& sndStar) $ input

cmd = [ "$ cd /"
      , "$ ls"
      , "dir gts"
      , "68377 jvdqjhr.jvp"
      , "dir lwhbw"
      , "228884 nqth.gcn"
      , "$ cd gts"
      , "$ ls"
      , "846 grwwbrgz.wft"
      , "$ cd .."
      , "$ cd lwhbw"
      , "$ ls"]
