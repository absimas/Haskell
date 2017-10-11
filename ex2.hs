import Data.Char

-- Task 1
average :: [Float] -> Float
average floats = sum floats / size
  where size = fromIntegral(length floats) :: Float

-- Task 2
divides :: Int -> [Int]
divides n
  | n < 0 = divides (-n)
  | otherwise = divisors n 1 []

divisors :: Int -> Int -> [Int] -> [Int]
divisors x y list
  | y > x = list
  | x `mod` y == 0 = divisors x (y+1) (list ++ [y])
  | otherwise = divisors x (y+1) list

divides2 :: Int -> [Int]
divides2 n = [x | x <- [1..n `div` 2], n `mod` x == 0] ++ [n]

is_prime :: Int -> Bool
is_prime n
  | n < 0 = error "Can only check non-negative numbers"
  | otherwise = divides2 n == [1, n]

-- Task 3
prefix :: String -> String -> Bool
prefix a b = a == take (length a) b

prefix2 :: String -> String -> Bool
prefix2 [] _ = True
prefix2 _ [] = False
prefix2 (x:xs) (y:ys) = (x == y) && prefix2 xs ys

-- Check if a is part of b
substring :: String -> String -> Bool
substring _ [] = False
substring a b
  | prefix a b == True = True
  | otherwise = substring a (tail b)

-- Task 4
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete x (y:ys)
  | x == y = ys
  | otherwise = y:delete x ys

permut :: [Integer] -> [Integer] -> Bool
permut [] [] = True
permut _ [] = False
permut [] _ = False
permut (x:xs) b = elem x b && permut xs (delete x b)

-- Task 5
isAlphabet :: Char -> Bool
isAlphabet c = elem c (['a'..'z'] ++ ['A'..'Z'])

toUppercase :: Char -> Char
toUppercase c
  | isAlphabet c && c >= 'a' = chr (ord c - 32)
  | otherwise = c

validate xs = [toUppercase x | x <- xs, isAlphabet x]

-- Task 6
-- First found pair with the same name has it's Integer value updated
-- While if the name isn't found a new pair element is added
add :: (String, Float) -> [(String, Float)] -> [(String, Float)]
add x [] = [x]
add x (y:ys)
  | fst x == fst y = (fst x, snd x + snd y):ys
  | otherwise = y : add x ys

itemTotal :: [(String, Float)] -> [(String, Float)]
itemTotal [] = []
itemTotal [x] = [x]
itemTotal (x:xs) = add x (itemTotal xs)

itemDiscount :: String -> Integer -> [(String, Float)] -> [(String, Float)]
itemDiscount _ _ [] = []
itemDiscount name discount (x:xs)
  | discount < 0 || discount > 100 = error "Discount should be specified in % ([0..100])!"
  | name == fst x = (name, snd x * (100 - fromIntegral(discount)) / 100) : itemDiscount name discount xs
  | otherwise = x : itemDiscount name discount xs
