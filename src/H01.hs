module H01 where

-- built-in
myLast xs = last xs

-- just using head and tail and managing the empty input list
myLast' [] = error "Empty list"
myLast' xs = if (tail xs == [])
  then head xs
  else myLast' (tail xs)

-- better handling the single element list avoid == operator
myLast'' [] = error "Empty list"
myLast'' xs = if null (tail xs)
  then head xs
  else myLast'' (tail xs)

-- using the reverse function
myLast''' = head . reverse

-- using the length function and accessing directly the list
myLast'''' [] = error "Empty list"
myLast'''' xs = xs !! (length xs -1)

-- using pattern matching
myLast''''' [] = error "Empty list"
myLast''''' [x] = x
myLast''''' (_:xs) = myLast''''' xs

-- using drop, same fashion of tail
myLast'''''' [] = error "Empty list"
myLast'''''' xs = if null (drop 1 xs)
  then head xs
  else myLast'''''' (drop 1 xs)

-- using foldl1
myLast''''''' xs = foldl1 (curry snd) xs

-- using foldr1
myLast'''''''' xs = foldr1 (flip const) xs


