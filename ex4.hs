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
