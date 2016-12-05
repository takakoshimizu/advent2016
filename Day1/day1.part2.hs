module Day1.Part2 where

import System.Environment
import Common


doMoves :: [Move] -> Position
doMoves = doMoves' [(0, 0)] (0, 0) (0, 1)
  where
    doMoves' :: [Position] -> Position -> Vector -> [Move] -> Position
    doMoves' _ (x, y) _ [] = (10000, 10000)
    doMoves' ps (x, y) (vx, vy) (m:ms) =
      case overlap of
        Just pos -> pos
        Nothing -> doMoves' newList newPos (vxx, vyy) ms
      where
        (distance, (vxx, vyy)) = case m of
          R x -> (x, (vy, -vx))
          L x -> (x, (-vy, vx))
        newPos = (x + vxx * distance, y + vyy * distance)
        newList = addAllPositions ps (x + vxx, y + vyy) newPos
        overlap = searchRepeating newList


addAllPositions :: [Position] -> Position -> Position -> [Position]
addAllPositions list cur to =
  list ++ [ (x, y) | x <- [(fst cur)..(fst to)], y <- [(snd cur)..(snd to)] ]


searchRepeating :: [Position] -> Maybe Position
searchRepeating [] = Nothing
searchRepeating (p:ps) =
  if p `elem` ps then Just p
  else searchRepeating ps


main :: IO ()
main = do
  args <- getArgs
  let (bx, by) = doMoves $ parseToMoves $ head args
  putStrLn $ show $ abs bx + abs by
