module AoC.Day9 where

import Control.Arrow ((&&&))
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace

type Input = [Move]
type Output = Int
type Coords = (Int, Int)
type Head = Coords
type Tail = Coords

data Move = U | D | L | R deriving (Eq, Enum, Show, Read)

initPosition :: Coords
initPosition = (0,0)

readLine :: String ->  [Move]
readLine (move:' ':no) = replicate (read no) (read [move])
readLine line          = error $ "Invalid input: " <> line

moveTo :: Coords -> Move -> Coords
moveTo (x,y) U = (x, y+1)
moveTo (x,y) D = (x, y-1)
moveTo (x,y) R = (x+1, y)
moveTo (x,y) L = (x-1, y)

followHead :: Head -> Tail -> Tail
followHead h@(hx, hy) t@(tx, ty) =
  let
    diffx = hx - tx
    diffy = hy - ty
    toLeft = signum diffx
    toUp = signum diffy
    shouldMoveDiag = if tx /= hx && ty /= hy && (abs diffx + abs diffy) > 2 then 1 else 0
    tx' = if abs diffx == 2 then tx + 1*toLeft else tx + 1*shouldMoveDiag*toLeft
    ty' = if abs diffy == 2 then ty + 1*toUp else ty + 1*shouldMoveDiag*toUp
  in {- trace (show h <> " | " <> show t <> " => " <> show (tx', ty')) -} (tx', ty')

simulate :: Input -> Output
simulate = length . fst . foldl' folder (S.empty, (initPosition, initPosition))
 where
  folder :: (Set Coords, (Head, Tail)) -> Move -> (Set Coords, (Head, Tail))
  folder (visited, (hd, tl)) mv =
    let headNewPos = moveTo hd mv
        tailNewPos = followHead headNewPos tl
    in (S.insert tailNewPos visited, (headNewPos, tailNewPos))

---

fstStar :: Input -> Output
fstStar = simulate

sndStar :: Input -> Output
sndStar = undefined

---

mainDay9 :: IO ()
mainDay9 = do
  input <- concatMap readLine . lines <$> readFile "src/input/day9"
  print . (fstStar &&& sndStar) $ input

--- playground
testInput2 = [ "R 3", "U 4"]
testInput = [ "R 4"
            , "U 4"
            , "L 3"
            , "D 1"
            , "R 4"
            , "D 1"
            , "L 5"
            , "R 2"
            ]

