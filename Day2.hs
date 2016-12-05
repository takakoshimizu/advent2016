module Day2 where

import System.Environment
import Data.List.Split
import Data.Char

type Position = (Int, Int)
type Direction = Char

keypad :: [[Int]]
keypad =
  [[1, 2, 3]
  ,[4, 5, 6]
  ,[7, 8, 9]]

keypadDiamond :: [[Char]]
keypadDiamond =
  [[' ', ' ', '1', ' ', ' ']
  ,[' ', '2', '3', '4', ' ']
  ,['5', '6', '7', '8', '9']
  ,[' ', 'A', 'B', 'C', ' ']
  ,[' ', ' ', 'D', ' ', ' ']]

checkBounds :: Int -> Int
checkBounds = checkMinBounds . checkMaxBounds
  where
    checkMinBounds = max 0
    checkMaxBounds = min 2

move :: Direction -> Position -> Position
move d (x,y) =
  case d of
    'U' -> (x, checkBounds $ y - 1)
    'D' -> (x, checkBounds $ y + 1)
    'L' -> (checkBounds $ x - 1, y)
    'R' -> (checkBounds $ x + 1, y)

moveDiamond :: Direction -> Position -> Position
moveDiamond d p@(x, y) =
  case d of
    'U' -> checkBoundsDiamond p (x, y - 1)
    'D' -> checkBoundsDiamond p (x, y + 1)
    'L' -> checkBoundsDiamond p (x - 1, y)
    'R' -> checkBoundsDiamond p (x + 1, y)

checkBoundsDiamond :: Position -> Position -> Position
checkBoundsDiamond from to@(x, y) =
  if toKeyDiamond boundedTo == ' ' then from
  else boundedTo
  where
    checkBounded = max 0 . min 4
    boundedTo = (checkBounded x, checkBounded y)

solveCode :: [Direction] -> Position
solveCode ds = solveCode' ds (1, 1)
  where
    solveCode' [] p = p
    solveCode' (d:ds) p = solveCode' ds $ move d p

solveCodeDiamond :: [Direction] -> Position
solveCodeDiamond ds = solveCodeDiamond' ds (0, 2)
  where
    solveCodeDiamond' [] p = p
    solveCodeDiamond' (d:ds) p = solveCodeDiamond' ds $ moveDiamond d p

solveCodes :: [[Direction]] -> [Position]
solveCodes = map solveCode

solveCodesDiamond :: [[Direction]] -> [Position]
solveCodesDiamond = map solveCodeDiamond

toKey :: Position -> Int
toKey (x, y) = keypad !! y !! x

toKeyDiamond :: Position -> Char
toKeyDiamond (x, y) = keypadDiamond !! y !! x

main :: IO ()
main = do
  args <- getArgs
  let moves = splitOn "\n" $ head args
  --putStrLn $ show $ map toKey $ solveCodes moves
  putStrLn $ show $ map toKeyDiamond $ solveCodesDiamond moves
