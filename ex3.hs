import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

-- Task 1
subst :: String -> String -> String -> String
subst _ _ [] = []
subst [] _ st = st
subst oldSub newSub (x:xs)
  | isPrefixOf oldSub (x:xs) = newSub ++ subst oldSub newSub oldSubRemoved
  | otherwise = x : subst oldSub newSub xs
  where
    oldSubRemoved = drop (length oldSub) (x:xs)

punctuation = " \t.,;-:"

-- Task 2
isPalin :: String -> Bool
isPalin st = isPalin' [toUpper x | x <- st, not (elem x punctuation)]
  where
    isPalin' [] = True
    isPalin' string
      | length string == 1 = False
      | first == last  = isPalin (tail (init string))
      | otherwise = False
      where
        first = head string
        last = string !! (length string - 1)

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

-- Circle overlaps circle
overlaps :: Shape -> Shape -> Bool
overlaps (Circle r0 p0) (Circle r1 p1) = dp <= dr
  where
    dp = distance p0 p1
    dr = r0 + r1

-- Rect overlaps rect
overlaps (Rectangle h0 w0 (Point x0 y0)) (Rectangle h1 w1 (Point x1 y1)) = x0 < x1+w1 && x0+w0 > x1 && y0+h0 > y1 && y0 < y1+h1
-- x0+w0 >= x1 && x1+w1 >= x0 && y0+h0 >= y1 || y1+h1 >= y0

-- Rect overlaps circle
overlaps (Rectangle h w p0) (Circle r p1) = overlaps (Circle r p1) (Rectangle h w p0)
overlaps (Circle r (Point x0 y0)) (Rectangle h w (Point x1 y1))
  | distX > (w/2 + r) || distY > (h/2 + r) = False
  | distX <= w/2 || distY <= h/2 = True
  | otherwise = dx^2 + dy^2 <= r^2
  where
    distX = abs (x0 - x1 - w/2)
    distY = abs (y0 - y1 - h/2)
    dx = distX - w/2
    dy = distY - h/2

-- Task 6
data Status = Loaned | Free | Locked
  deriving (Show, Ord, Eq)

data Book = Book { name :: String, bookId :: Int, status :: Status }
  deriving (Show, Ord, Eq)

data Person = Person { firstName :: String, loans :: [Book] }
  deriving (Show, Ord, Eq)

-- Compare by name and id only?
-- instance Eq Book
--   where
--     (Book name0 id0 _) == (Book name1 id1 _) = name0 == name1 && id0 == id1
-- instance Eq Person
--   where
--     (Person firstName0 _) == (Person firstName1 _) = firstName0 == firstName1

books = [(Book "book1" 0 Free), (Book "book1" 1 Loaned), (Book "book3" 2 Locked), (Book "book4" 3 Free)]
persons = [Person "person1" [], Person "person2" []]

loan :: Person -> Book -> ([Book], [Person]) -> ([Book], [Person])
loan person book (books,people)
  | personIndex == Nothing = error "Person is not in the database!"
  | bookIndex == Nothing = error "Book is not in the database!"
  | bookStatus == Locked || bookStatus == Loaned = (books, people)
  | otherwise = (modifiedBooks, modifiedPeople)
  where
    personIndex = elemIndex person people
    bookIndex = elemIndex book books
    bookStatus = status book
    bookSplit = splitAt ((fromJust bookIndex)+1) books
    peopleSplit = splitAt ((fromJust personIndex)+1) people
    modifiedBook = Book (name book) (bookId book) Loaned
    modifiedPerson = Person (firstName person) (modifiedBook : (loans person))
    modifiedBooks = (init (fst bookSplit)) ++ [modifiedBook] ++ snd bookSplit
    modifiedPeople = (init (fst peopleSplit)) ++ [modifiedPerson] ++ snd peopleSplit
