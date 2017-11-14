-- Exercise 1
map_length = sum . map (\_->1)
foldr_length arr = foldr (\_ n -> 1+n) 0 arr
