import Data.Char (digitToInt)
import Test.QuickCheck
import Data.List

exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not( x && y)

-- Task 1
nAnd :: Bool -> Bool -> Bool
nAnd2 :: Bool -> Bool -> Bool
nAnd x y = not(x && y)
nAnd2 True x = not(x)
nAnd2 False x = True

-- Task 2
prop_nAnds :: Bool -> Bool -> Bool
prop_nAnds x y = nAnd x y == nAnd2 x y

-- TODO
-- Think of one more property that any implementation of nAnd must satisfy
-- (for example, that it should return True if any of the arguments is
-- False) and test it.

-- Task 3
-- Function to convert Integer to a list containing all digits
toList :: Integer -> [Integer]
toList = map(fromIntegral.digitToInt).show

-- Count digits in an integer (ignores minus sign)
nDigits :: Integer -> Int
nDigits n
  | n < 0 = genericLength(toList(n))-1
  | otherwise = genericLength(toList(n))
