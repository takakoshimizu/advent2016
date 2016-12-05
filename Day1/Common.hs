module Common where

import Data.List.Split

data Move = R Int | L Int deriving (Show, Read)
type Vector = (Int, Int)
type Position = (Int, Int)


parseToMoves :: String -> [Move]
parseToMoves x =
  map parseMove $ splitOn ", " $ x


parseMove :: String -> Move
parseMove (d:n) = read $ d : ' ' : n :: Move
