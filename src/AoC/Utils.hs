module AoC.Utils (splitWhen) where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen = go []
 where
  go ys _ [] = [ys]
  go ys p (x:xs)
    | p x = ys:(go [] p xs)
    | otherwise  = go (x:ys) p xs
