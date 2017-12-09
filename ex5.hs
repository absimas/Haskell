import qualified Data.Set as S

-- Exercise 1
data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
type Ops a = [a] -> a
type Var = Char

type Valuation a = [(Var, a)]
eval :: Valuation a -> Expr a -> a
eval _ (Lit n) = n
eval valuations (EVar var) = case lookup var valuations of
  Just a -> a
  Nothing -> error "Variable not found!"
eval valuations (Op ops expressions) = ops mapped
  where
    mapped = map (\expression -> eval valuations expression) expressions

-- eval [('x', 5), ('y', 7)] (Op sum [Op product [Op sum [EVar 'x', Lit 4], EVar 'y'], Lit 1])

-- Exercise 2
average xs  = sum xs / fromIntegral(length xs)
data NumList a = Nlist [a]
instance (Fractional a, Eq a) => Eq (NumList a) where
  (Nlist a)  == (Nlist b) = average a == average b
instance (Fractional a, Ord a) => Ord (NumList a) where
  (Nlist a)  <= (Nlist b) = average a <= average b

-- Exercise 3
instance (Num a, Num b) => Num (a->b) where
  a + b = \x -> a x + b x
  a * b = \x -> a x * b x
  negate a = negate . a
  abs a = abs . a
  signum a = signum . a
  fromInteger a = \x -> fromInteger a

-- Exercise 4
data GTree a = Leaf a | Gnode [GTree a] deriving (Show)
depth :: GTree a -> Int
depth (Leaf _) = 1
depth (Gnode trees) = 1 + foldr (\tree n -> max n (depth tree)) 0 trees

contains :: Eq a => GTree a -> GTree a -> Bool
contains (Leaf a) (Leaf b) = a == b
contains (Leaf leaf) (Gnode trees) = any (contains (Leaf leaf)) trees
contains (Gnode trees) (Leaf leaf) = False
contains (Gnode a) (Gnode b) = False

mapTree :: (a->b) -> GTree a -> GTree b
mapTree f (Gnode trees) = Gnode (map (\tree -> mapTree f tree) trees)
mapTree f (Leaf leaf) = Leaf (f leaf)

-- Exercise 5
data Result a = OK a | Error String
composeResult :: (a -> Result b) -> (b-> Result c) -> (a -> Result c)
composeResult f g a = case f a of
  Error error -> Error error
  OK ok -> case g ok of
    Error error -> Error error
    OK ok2 -> g ok

-- Exercise 6
type Relation a b = S.Set (a,b)
dom :: Ord a => Relation a b -> S.Set a
dom xs = S.fromList [fst a | a <- first]
  where
    first = S.toList xs
-- dom S.fromList([(1,2), (1,3), (2,4)]) = [1,2]

ran :: Ord b => Relation a b -> S.Set b
ran xs = S.fromList [snd a | a <- first]
  where
    first = S.toList xs
-- ran S.fromList([(1,2), (1,3), (2,4)]) = [2,3,4]

image :: (Ord a, Ord b) => S.Set a -> Relation a b -> S.Set b
image xs ys = S.fromList [b | (a,b) <- second, elem a first]
  where
    first = S.toList xs
    second = S.toList ys

-- Exercise 7
primes :: [Integer]
primes = sieve [2..]
sieve (x:xs) = x : sieve [y | y <- xs, y `mod` x > 0]

goldbach :: Integer -> Bool
goldbach n = all (\x -> elem x sums) evens
  where
    _primes = takeWhile (\x -> x <= n) primes
    sums = [x+y | x <- _primes, y <- _primes]
    evens = [x | x <- [4..n], even x]

-- Exercise 8
data Stream a = Cons a (Stream a)

streamToList ::  Stream a -> [a]
streamToList (Cons a stream) = a : streamToList stream
result1 = take 5 (streamToList naturals)
  where
    naturals = _naturals 1
    _naturals n = Cons n (_naturals (n+1))

streamIterate :: (a -> a) -> a -> Stream a
streamIterate f seed = Cons seed (streamIterate f (f seed))
result2 = take 5 (streamToList (streamIterate (\x -> x + 1) 5))

streamInterleave ::  Stream a -> Stream a -> Stream a
streamInterleave (Cons a stream1) stream2 = Cons a (streamInterleave stream2 stream1)
result3 = take 10 (streamToList (streamInterleave stream1 stream2))
  where
    stream1 = streamIterate (\x -> x + 1) 1
    stream2 = streamIterate (\x -> x + 1) 11
