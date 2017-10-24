import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

subst :: String -> String -> String -> String
subst _ _ [] = []
subst [] _ st = st
subst oldSub newSub (x:xs)
  | isPrefixOf oldSub (x:xs) = newSub ++ subst oldSub newSub oldSubRemoved
  | otherwise = x : subst oldSub newSub xs
  where
    oldSubRemoved = drop (length oldSub) (x:xs)

-- Task 2
isPalin :: String -> Bool
isPalin [] = True
isPalin string
  | notLetter first = isPalin (tail string)
  | notLetter last = isPalin (init string)
  | length string == 1 = False
  | first == last  = isPalin (tail (init string))
  | otherwise = False
  where
    first = toUpper (head string)
    last = toUpper (string !! (length string - 1))
    notLetter x = not (isLetter x)

-- Task 3
count :: String -> (Int, Int, Int)
count [] = (0,0,0)
count st = (chars, words, lines)
  where
    chars = length st
    words = length (splitOn " " st)
    lines = length (splitOn "\n" st)

-- Following doesn't work for "abc" - should have 1 word and 1 line
-- count (x:xs)
--   | x == ' ' && (length xs == 0 || head xs == ' ') = sumTuple (1, 0, 0) (count xs)
--   | x == ' ' = sumTuple (1, 1, 0) (count xs)
--   | x == '\n' = sumTuple (1, 0, 1) (count xs)
--   | otherwise = sumTuple (1, 0, 0) (count xs)
--   where
--     sumTuple (a,b,c) (d,e,f) = (a+d, b+e, c+f)

-- Task 4
justify :: String -> Int -> String
justify [] _ = []
justify st i
  | i >= length st = st
  | otherwise = fst split ++ "\n" ++ justify (snd split) i
  where
    preSpaceIndex = findBackwards ' ' st i
    split = splitAt (preSpaceIndex+1) st

-- findBackwards criteria string startIndex
-- finds 'criteria' within 'string' starting from 'startIndex' (inclusive)
findBackwards :: Char -> String -> Int -> Int
findBackwards _ [] _ = error "Given string is empty!"
findBackwards c st i
  | i >= length st = error "Given index is OOB!"
  | i < 0 = error "Space not found in string ? starting at index ?"
  | c == (st !! i) = i
  | otherwise = findBackwards c st (i-1)
