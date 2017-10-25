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
-- splitBy - splits string (2nd arg) by any of the given chars (1st arg)
count :: String -> (Int, Int, Int)
count [] = (0,0,0)
count st = (chars, words, lines)
  where
    chars = length st
    words = length (splitOneOf punctuation st)
    lines = length (splitOn "\n" st)

-- Task 4
justify :: String -> Int -> String
justify [] _ = []
justify st i
  | i >= length st = st
  | otherwise = fst split ++ "\n" ++ justify (snd split) i
  where
    preSpaceIndex = findBackwards punctuation st i
    split = splitAt (preSpaceIndex+1) st

-- findBackwards criteria string startIndex
-- finds any char from 'criteria' within 'string' starting from 'startIndex' (inclusive)
findBackwards :: String -> String -> Int -> Int
findBackwards _ [] _ = error "Given string is empty!"
findBackwards criteria st i
  | i >= length st = error "Given index is OOB!"
  | i < 0 = error "Punctuation not found!"
  | elem (st !! i) criteria = i
  | otherwise = findBackwards criteria st (i-1)

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

type Book = String
type Person = String

data BookEntity = BookEntity { name :: String, bookId :: Int, status :: Status }
  deriving (Show, Ord, Eq)

data PersonEntity = PersonEntity { firstName :: String, loans :: [BookEntity] }
  deriving (Show, Ord, Eq)

books = [(BookEntity "book1" 0 Free), (BookEntity "book1" 1 Loaned), (BookEntity "book3" 2 Locked), (BookEntity "book4" 3 Free)]
people = [PersonEntity "person1" [], PersonEntity "person2" []]

loan :: Person -> Book -> ([PersonEntity], [BookEntity]) -> ([PersonEntity], [BookEntity])
loan personName bookName (people, books)
  | personEntity == Nothing = error "Person is not in the database!"
  | null bookEntities = error "Book is not in the database!"
  | bookEntity == Nothing = (people, books)
  | otherwise = (modifiedPeople, modifiedBooks)
  where
    personEntity = find (\p -> firstName p == personName) people
    personEntity' = fromJust personEntity
    bookEntities = [b | b <- books, name b == bookName]
    bookEntity = find (\b -> status b == Free) bookEntities
    bookEntity' = fromJust bookEntity
    bookIndex = elemIndex bookEntity' books
    personIndex = elemIndex personEntity' people
    bookSplit = splitAt ((fromJust bookIndex)+1) books
    peopleSplit = splitAt ((fromJust personIndex)+1) people
    modifiedBook = BookEntity (name bookEntity') (bookId bookEntity') Loaned
    modifiedPerson = PersonEntity (firstName personEntity') (modifiedBook : (loans personEntity'))
    modifiedBooks = (init (fst bookSplit)) ++ [modifiedBook] ++ snd bookSplit
    modifiedPeople = (init (fst peopleSplit)) ++ [modifiedPerson] ++ snd peopleSplit
