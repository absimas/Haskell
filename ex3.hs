import Data.Char
import Data.List

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
count (x:xs) -- ToDo doesn't work for "abc" - should have 1 word
  | x == ' ' && (length xs == 0 || head xs == ' ') = sumTuple (1, 0, 0) (count xs)
  | x == ' ' = sumTuple (1, 1, 0) (count xs)
  | x == '\n' = sumTuple (1, 0, 1) (count xs)
  | otherwise = sumTuple (1, 0, 0) (count xs)
  where
    sumTuple (a,b,c) (d,e,f) = (a+d, b+e, c+f)

nOccurs ::  [Integer] -> [(Integer,Int)]
nOccurs [] = []
nOccurs (x:xs) = (x, length onlyX + 1) : (nOccurs withoutX)
  where
    onlyX = [xx | xx <- xs, xx == x]
    withoutX = [xx | xx <- xs, xx /= x]
