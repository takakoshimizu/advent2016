module Day3 where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.Text as T

type Triangle = (Int, Int, Int)

isValidTriangle :: Triangle -> Bool
isValidTriangle (x, y, z) =
  x + y > z &&
  x + z > y &&
  z + y > x

toTriangle :: String -> Triangle
toTriangle str =
  (x, y, z)
  where
    toInt x = read x :: Int
    [x, y, z] = map toInt $ filter (not . null) $ map stripStr $ splitOn " " str

stripStr :: String -> String
stripStr = T.unpack . T.strip . T.pack

main :: IO ()
main = do
  items <- readFile "day3.txt"
  let triangles = map (toTriangle . stripStr) $ filter (not . null) $ splitOn "\n" items
  putStrLn $ show $ length (filter isValidTriangle triangles)
