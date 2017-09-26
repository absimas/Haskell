import Data.Char (digitToInt)
import Test.QuickCheck
import Data.List

-- Task 1
nAnd :: Bool -> Bool -> Bool
nAnd2 :: Bool -> Bool -> Bool
nAnd x y = not(x && y)
nAnd2 True x = not(x)
nAnd2 False x = True

-- Task 2
prop_nAnds :: Bool -> Bool -> Bool
prop_nAnds x y = nAnd x y == nAnd2 x y

prop_nAnds2 :: Bool -> Bool -> Bool
prop_nAnds2 x y = (nAnd False y ==  nAnd x False) == True

-- Task 3
-- Function to convert Integer to a list containing all digits
toList :: Integer -> [Integer]
toList = map(fromIntegral.digitToInt).show

-- Count digits in an integer (ignores minus sign)
nDigits :: Integer -> Int
nDigits n
  | n < 0 = genericLength(toList(n))-1
  | otherwise = genericLength(toList(n))

-- Task 4
nRoots :: Float -> Float -> Float -> Int
nRoots a b c
  | a == 0.0 = error "The first argument should not be zero!"
  | b^2 > 4.0 * a * c = 2
  | b^2 == 4.0 * a * c = 1
  | b^2 < 4.0 * a * c = 0

-- Task 5
signedRoot :: Int -> Float -> Float -> Float -> Float
signedRoot s a b c
  | nRoots a b c == 0 = error "No roots!"
  | first >= second && s > 0 || first < second && s < 0 = first -- daijoubou ka?
  | otherwise = second
  where
    root = sqrt(b^2 - 4 * a * c) :: Float
    first = ((-b) + root) / (2*a)
    second = ((-b) - root) / (2*a)

sRoot :: Float -> Float -> Float -> Float
sRoot a b c = signedRoot (-1) a b c

lRoot :: Float -> Float -> Float -> Float
lRoot a b c = signedRoot 1 a b c

-- Task 6
power2 :: Integer -> Integer
power2 n
  | n < 0 = 0
  | n == 0 = 1
  | otherwise = 2 * power2(n-1)

-- Task 7
mult :: Integer -> Integer -> Integer
mult m n
  | n == 0 = 0
  | n < 0 = (-mult m (-n))
  | otherwise = m + mult m (n - 1)

-- Task 8
prod :: Integer -> Integer -> Integer
prod m n
  | m > n = error "Invalid range."
  | m - n == 0 = n
  | otherwise = n * prod m (n - 1)

fac :: Integer -> Integer
fac n
  | n == 0 = 1
  | n > 0 = prod 1 n
  | otherwise = error "Negaqtive argument!"
