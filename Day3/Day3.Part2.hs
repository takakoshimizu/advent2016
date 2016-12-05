module Day3 where

import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.List (transpose)
import qualified Data.Text as T

type Triangle = (Int, Int, Int)

isValidTriangle :: Triangle -> Bool
isValidTriangle (x, y, z) =
  x + y > z &&
  x + z > y &&
  z + y > x

toInts :: String -> [Int]
toInts = (map read) . (filter (not . null)) . (splitOn " ")

toTriangles :: [Int] -> [Triangle]
toTriangles [] = []
toTriangles xs = toTriangle triple : toTriangles next
  where
    triple = take 3 xs
    next = drop 3 xs

toTriangle :: [Int] -> Triangle
toTriangle xs = (xs !! 0, xs !! 1, xs !! 2)

strip :: String -> String
strip = T.unpack . T.strip . T.pack

main :: IO ()
main = do
  items <- readFile "day3.txt"
  let list = (map $ toInts . strip) . (filter (not . null)) . (splitOn "\n") $ items
  let columns = concat . transpose $ list
  let triangles = toTriangles columns
  putStrLn $ show $ length $ filter isValidTriangle triangles
