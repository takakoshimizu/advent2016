module Day8 where

import Data.List (transpose, intercalate)
import Data.List.Split (splitOn)

type Screen = [[Bool]]
data Cmd
  = Rect (Int, Int)
  | RotateColumn Int Int
  | RotateRow Int Int
  deriving (Show)

screenDisplay :: Screen -> String
screenDisplay s =
  intercalate "\n" $ map displayRow s
    where
      toString b = if b then '#' else '_'
      displayRow r = map toString r

initScreen :: Screen
initScreen = replicate 6 (replicate 50 False)

lightRow :: [Bool] -> Int -> [Bool]
lightRow r x =
  (replicate x True) ++ (drop x r)

replaceRow :: Screen -> [Bool] -> Int -> Screen
replaceRow s r y =
  before ++ [r] ++ after
  where
    (before, _:after) = splitAt y s

rect :: (Int, Int) -> Screen -> Screen
rect (x, y) s = rect' s (x, y - 1)
  where
    rect' s (_, -1) = s
    rect' s (x, y) =
      rect' newScreen (x, y -1)
      where
        newRow = lightRow (s !! y) x
        newScreen = replaceRow s newRow y

rotateRow :: Int -> Int -> Screen -> Screen
rotateRow y by s =
  replaceRow s newRow y
    where
      oldRow = s !! y
      (before, after) = splitAt ((length oldRow) - by) oldRow
      newRow = after ++ before

rotateColumn :: Int -> Int -> Screen -> Screen
rotateColumn x by =
  transpose . (rotateRow x by) . transpose

parseCmd :: String -> Cmd
parseCmd s =
  case c of
    "rect" -> Rect (parseRect type')
    "rotate" ->
      case type' of
        "row" -> RotateRow x a
        "column" -> RotateColumn x a
        where
          (_:xx:_) = splitOn "=" (rest !! 0)
          x = read xx :: Int
          a = read (rest !! 2) :: Int
    where
      (c:type':rest) = words s
      parseRect type' = (read x, read y)
        where
          (x:y:_) = splitOn "x" type'

doCmd :: Screen -> Cmd -> Screen
doCmd s c =
  case c of
    Rect coords -> rect coords s
    RotateRow y by -> rotateRow y by s
    RotateColumn x by -> rotateColumn x by s

main :: IO ()
main = do
  input <- readFile "day8.txt"
  let cmds = map parseCmd $ filter (not . null) $ splitOn "\n" input
  let finalScreen = foldl doCmd initScreen cmds
  let litPixels = length $ (filter id) . concat $ finalScreen
  putStrLn $ screenDisplay finalScreen
  putStrLn $ show litPixels
