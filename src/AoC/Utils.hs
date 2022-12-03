module AoC.Utils (splitWhen) where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen = go []
 where
  go yss _ [] = yss
  go [] p (x:xs)
    | p x = go [] p xs
    | otherwise   = go [[x]] p xs
  go l@(ys:yss) p (x:xs)
    | p x = go ([]:l) p xs
    | otherwise  = go ((x:ys):yss) p xs


