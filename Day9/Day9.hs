module Day9 where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Extra (eol)

marker :: Parser String
marker = do
  char '('
  x <- many (noneOf "x")
  char 'x'
  y <- many (noneOf ")")
  char ')'
  contents <- count (read x) anyChar
  return $ concat $ replicate (read y) contents

markerv2 :: Parser Int
markerv2 = do
  char '('
  x <- many (noneOf "x")
  char 'x'
  y <- many (noneOf ")")
  char ')'
  contents <- count (read x) anyChar
  let expanded = parse (manyTill (markerv2 <|> charv2) (eol <|> eof)) "" contents
  let recurse = (either (const 0) (foldl (+) 0)) expanded
  return $ (read y) * recurse

charv2 :: Parser Int
charv2 = do
  x <- many alphaNum
  return $ length x

file :: Parser String
file = do
  bits <- manyTill (marker <|> many alphaNum) (eol <|> eof)
  return $ concat bits

filev2 :: Parser Int
filev2 = do
  bits <- manyTill (markerv2 <|> charv2) (eol <|> eof)
  return $ foldl (+) 0 bits

parseString :: String -> String
parseString =
  (either (const "ERR") id) . (parse file "")

parseStringV2 :: String -> Int
parseStringV2 =
  (either (const (-1)) id) . (parse filev2 "")

main :: IO ()
main = do
  file <- readFile "day9.txt"
  let s = parseString file
  let l = parseStringV2 file
  putStrLn $ "Uncompressed length is: " ++ (show $ length s)
  putStrLn $ "V2 Uncompressed length is: " ++ (show l)
