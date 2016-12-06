module Day6 where

import Data.List (nub, minimumBy, transpose)
import Data.Function (on)

leastCommon :: Ord a => [a] -> Maybe a
leastCommon [] = Nothing
leastCommon list = Just $ fst $ minimumBy (compare `on` snd) elemCounts
  where
    elemCounts = nub [(e, c) | e <- list, let c = length (filter (== e) list)]

main :: IO ()
main = do
  input <- readFile "day6.txt"
  let columns = transpose . words $ input
  let commons = map ((maybe '-' id) . leastCommon) columns
  putStrLn $ show commons
