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

-- Task 5
data Point = Point Float Float
  deriving (Show, Ord, Eq)

distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

data Line = Line Point Point
  deriving (Show, Ord, Eq)

data Shape = Circle Float Point | Rectangle Float Float Point
  deriving (Show, Ord, Eq)

-- Check if first point is between the other 2
intersects :: Point -> Point -> Point -> Bool
intersects p1 p2 p3 = (distance p1 p2) + (distance p1 p3) == (distance p2 p3)

-- Circle within circle
overlaps :: Shape -> Shape -> Bool
overlaps (Circle r0 p0) (Circle r1 p1) = dp <= dr
  where
    dp = distance p0 p1
    dr = r0 + r1
-- Rect within rect
overlaps (Rectangle h0 w0 (Point x0 y0)) (Rectangle h1 w1 (Point x1 y1)) = x0+w0 >= x1 && x1+w1 >= x0 && y0+h0 >= y1 || y1+h1 >= y0

-- Rect within circle / circle within rect
overlaps (Rectangle h w p0) (Circle r p1) = overlaps (Circle r p1) (Rectangle h w p0)
overlaps (Circle r (Point x0 y0)) (Rectangle h w (Point x1 y1))
  | distX > (w/2 + r) || distY > (h/2 + r) = False
  | otherwise = True
  where
    distX = abs ((x0) - ((x1) - (w/2)))
    distY = abs ((y0) - ((y1) - (h/2)))
