module AoC.Utils (splitWhen, splitOn, chunksOf) where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen = go []
 where
  go ys _ [] = [reverse ys]
  go ys p (x:xs)
    | p x = (reverse ys):(go [] p xs)
    | otherwise  = go (x:ys) p xs

splitOn :: Char -> String -> [String]
splitOn c = splitWhen (== c)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs =
  let (x1, x2) = splitAt n xs
  in x1:(chunksOf n x2)

