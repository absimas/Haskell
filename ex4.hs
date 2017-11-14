-- Exercise 1
map_length = sum . map (\_-> 1)
foldr_length arr = foldr (\_ n -> 1+n) 0 arr

-- Exercise 2
any1 :: (a->Bool) -> [a] -> Bool
all1 :: (a->Bool) -> [a] -> Bool

any1 cond arr = (length $ filter cond arr) >  0
all1 cond arr = (length $ filter cond arr) == length arr

any2 :: (a->Bool) -> [a] -> Bool
all2 :: (a->Bool) -> [a] -> Bool

any2 cond arr = foldr (\x n -> n || cond x) False arr
all2 cond arr = foldr (\x n -> n && cond x) True arr

-- Exercise 3
unzip :: [(a,b)] -> ([a],[b])
unzip arr = foldr (\pair lists -> (fst pair : fst lists, snd pair : snd lists)) ([], []) arr

-- Exercise 4
ff :: Integer -> [Integer] -> Integer
ff bound = foldr sum 0 . map multiply . filter notNegative
  where
    notNegative x = x >= 0
    multiply x = x * 10
    sum elem current
      | elem + current >= bound = bound
      | otherwise = elem + current

-- Exercise 5
flip :: (a->b->c) -> (b->a->c)
flip f x y = f y x

-- Exercise 6
total :: (Integer -> Integer) -> (Integer -> Integer)
total f g = foldr (\x n -> n + f x) 0 [0..g]

-- Exercise 7
iter1 :: Integer -> (a->a) -> (a->a)
iter1 n f
  | n <= 0 = id
  | otherwise = f . (iter1 (n-1) f)

iter2 :: Int -> (a->a) -> (a->a)
iter2 n f = foldr (\x n -> x . n) id (replicate n f)

-- Exercise 8
splits :: [a] -> [([a], [a])]
splits arr = foldr (\x n -> n ++ [splitAt (length n+1) arr]) [([], arr)] arr
