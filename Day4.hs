module Day4 where

import Data.List (intercalate, sort)
import Data.List.Split (splitOn)

data Room = Room
  { name :: String
  , sector :: Int
  , checkSum :: String }
  deriving (Show)

toRoom :: String -> Room
toRoom str =
  Room { name = name
  , sector = sector
  , checkSum = checkSum }
  where
    parts = splitOn "[" str
    roomStuff = splitOn "-" $ head parts
    name = intercalate "-" $ init roomStuff
    sector = read $ last roomStuff
    checkSum = init $ last parts

countLetter :: String -> Char -> Int
countLetter s c =
  (length . filter ((==) c)) s

countLetters :: String -> String -> [Int]
countLetters room =
  map (countLetter room)

containsHighest :: [Int] -> [Int] -> Bool
containsHighest all cs =
  (((take 5) . reverse . sort) all) == cs

succCycled :: Char -> Char
succCycled c | cycled == '{' = 'a'
             | otherwise = cycled
  where cycled = succ c

cycleLetter :: Int -> Char -> Char
cycleLetter _ '-' = ' '
cycleLetter 0 c = c
cycleLetter n c = cycleLetter (n - 1) (succCycled c)

cycleLetters :: Int -> (String -> String)
cycleLetters times =
  map (cycleLetter times)

main :: IO ()
main = do
  input <- readFile "day4.txt"
  let rooms = map toRoom $ words input
  let validRooms = filter (\r -> containsHighest (countLetters (name r) ['a'..'z']) (countLetters (name r) (checkSum r))) rooms
  let translated = map (\r -> r { name = cycleLetters (sector r) (name r) }) validRooms
  let roomWithNpObjects = filter (\r -> "northpole object storage" == (name r)) translated
  putStrLn $ show roomWithNpObjects
