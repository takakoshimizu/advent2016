module Day5 where

import Data.Hash.MD5 (md5s, Str(..))
import Data.Sequence (fromList, update)
import Data.Foldable (toList)
import Data.Char (isSpace, isDigit)

hash :: String -> String
hash = md5s . Str

isHashOfInterest :: String -> Bool
isHashOfInterest s =
  ((== "00000") . (take 5)) s
  && hasConstrainedIndex s
  where
    hasValidIndex = isDigit . (!! 5)
    hasConstrainedIndex = (`elem` ['0'..'7']) . (!! 5)

hashUntilInteresting :: String -> Int -> (String, Int)
hashUntilInteresting s i =
  if isHashOfInterest thisHash then (thisHash, i)
  else hashUntilInteresting s (i + 1)
  where
    hashIndex s i = hash $ s ++ (show i)
    thisHash = hashIndex s i

extractPasswordChar :: String -> (Int, Char)
extractPasswordChar s = (read [(s !! 5)], s !! 6)

crackPassword :: String -> String
crackPassword id = crackPassword' 0 id (replicate 8 ' ')
  where
    crackPassword' i s p
      | isValidPassword ppp = ppp
      | otherwise = crackPassword' (ii + 1) s ppp
        where
          isValidPassword p = not $ any isSpace p
          (pp, ii) = hashUntilInteresting s i
          (pix, pc) = extractPasswordChar pp
          ppp =
            if (not . isSpace . (!! pix)) p then p
            else toList $ update pix pc $ fromList p
