import Data.List

-- Task 1
average :: [Float] -> Float
average floats = sum floats / size
  where size = fromIntegral(length floats) :: Float

-- Task 2
divides :: Int -> [Int]
divides n = divisors n 1 []

divisors :: Int -> Int -> [Int] -> [Int]
divisors x y list
  | y > x = list
  | x `mod` y == 0 = divisors x (y+1) (list ++ [y])
  | otherwise = divisors x (y+1) list

divides2 :: Int -> [Int]
divides2 n = [x | x <- [1..n], n `mod` x == 0]

is_prime :: Int -> Bool
is_prime n
  | n < 0 = error "Can only check non-negative numbers"
  | otherwise = divides2 n == [1, n]

-- Task 3
prefix :: String -> String -> Bool
prefix a b = a == take (length a) b

-- Check if a is part of b
substring :: String -> String -> Bool
substring a b
  | prefix a b == True = True
  | null b = False
  | otherwise = substring a (tail b)

-- Task 4
-- Delete 'a' elements from 'b'
permut :: [Integer] -> [Integer] -> Bool
permut a b
  | length a > 0 && elem removal b = permut (tail a) (delete removal b)
  | otherwise = length a == 0
  where
   removal = head a

-- Sort and compare lists
permut2 :: [Integer] -> [Integer] -> Bool
permut2 a b
  | length a /= length b = False
  | otherwise = sort a == sort b

-- Task 5
validate xs = [x | x <- xs, v <- ['a'..'z'] ++ ['A'..'Z'], x==v]
