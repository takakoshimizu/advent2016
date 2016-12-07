module Day7 where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Extra (eol)
import Data.List (isInfixOf)

data Nets
  = Subnet String
  | Hypernet String
  deriving (Show)

extract :: Nets -> String
extract n =
  case n of
    Subnet x -> x
    Hypernet x -> x

isHyper :: Nets -> Bool
isHyper n =
  case n of
    Hypernet _ -> True
    otherwise -> False

hypernet :: Parser Nets
hypernet = do
  string "["
  net <- many (noneOf "]")
  string "]"
  return $ Hypernet net

subnet :: Parser Nets
subnet = do
  net <- many (noneOf "[")
  return $ Subnet net

ipAddress :: Parser [Nets]
ipAddress = do
  try $ manyTill ip7 end
  where
    ip7 = hypernet <|> subnet
    end = eol <|> eof

parseIpAddress :: String -> Either ParseError [Nets]
parseIpAddress =
  parse ipAddress ""

hasAbba :: String -> Bool
hasAbba s = hasAbba' "    " s
  where
    hasAbba' _ [] = False
    hasAbba' ls (x:xs) =
      if hasOne then True
      else hasAbba' newls xs
      where
        newls = (tail ls) ++ [x]
        hasOne = newls !! 0 == newls !! 3
          && newls !! 1 == newls !! 2
          && newls !! 0 /= newls !! 1

findAbas :: String -> [String]
findAbas s = findAbas' [] "   " s
  where
    findAbas' xs _ [] = xs
    findAbas' is ls (x:xs) =
      if hasOne then findAbas' (is ++ [newls]) newls xs
      else findAbas' is newls xs
      where
        newls = (tail ls) ++ [x]
        hasOne = newls !! 0 == newls !! 2
          && newls !! 0 /= newls !! 1

containsAnyAba :: String -> [String] -> Bool
containsAnyAba net abas =
  (> 0) . length . (filter (flip isInfixOf $ net)) $ abas

part1 :: [[Nets]] -> Int
part1 nets = length $ filter isValid nets
  where
    isValid n = (length subs > 0) && (length hypers == 0)
      where
        abbas = filter (hasAbba . extract) n
        hypers = filter isHyper abbas
        subs = filter (not . isHyper) abbas

part2 :: [[Nets]] -> Int
part2 nets = length $ filter isValid nets
  where
    isValid n = hasOne
      where
        subs = filter (not . isHyper) n
        hypers = filter isHyper n
        abas = concat . (map $ findAbas . extract) $ subs
        babs = map ((take 3) . cycle . (drop 1)) abas
        hasOne = (> 0) . length . (filter $ (flip containsAnyAba $ babs) . extract) $ hypers

main :: IO ()
main = do
  input <- readFile "day7.txt"
  let nets = (map ((either (const []) id) . parseIpAddress)) . words $ input
  putStrLn $ "Part1: " ++ (show $ part1 nets)
  putStrLn $ "Part2: " ++ (show $ part2 nets)
