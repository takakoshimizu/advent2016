module Day1.Part1 where

import System.Environment
import Common


doMoves :: [Move] -> Position
doMoves = doMoves' (0, 0) (0, 1)
  where
    doMoves' :: Position -> Vector -> [Move] -> Position
    doMoves' (x, y) _ [] = (x, y)
    doMoves' (x, y) (vx, vy) (m:ms) =
      doMoves' newPos (vxx, vyy) ms
      where
        (distance, (vxx, vyy)) = case m of
          R x -> (x, (-vy, vx))
          L x -> (x, (vy, -vx))
        newPos = (x + vxx * distance, y + vyy * distance)


main :: IO ()
main = do
  args <- getArgs
  let (bx, by) = doMoves $ parseToMoves $ head args
  putStrLn $ show $ abs bx + abs by
