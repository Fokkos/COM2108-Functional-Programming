{-
  Week 3
  James March, 11/10/2022
-}


module Week3 where
    {-
        Q1 What function would you pass as an argument to mergesort in order to sort a list
        of (name, grade) pairs by grade order, from lowest to highest? (Can you use a
        library function, or do you need to write your own function? If you need to write
        your own function, write it.)
    -}

    {- mergesort - as a higher order function, presented in lectures 8/9 2022
   mergesort & merge provided by Emma Norling -}

    mergesort :: (Ord a, Show a) => (a->a->Bool) -> [a] -> [a]
    mergesort cmp [] = []
    mergesort cmp [x] = [x]
    mergesort cmp xs
      = merge cmp (mergesort cmp ys) (mergesort cmp zs)
        where 
        (ys, zs) = (take n xs, drop n xs)
        n = length xs `div` 2

    merge :: (Ord a, Show a) => (a->a->Bool) -> [a] -> [a] -> [a]
    merge cmp [] ys = ys
    merge cmp xs [] = xs
    merge cmp (x:xs) (y:ys)
        | cmp x y = x : merge cmp xs (y:ys)
        | otherwise = y : merge cmp (x:xs) ys

    mergeFunction :: (a, Int) -> (a, Int) -> Bool
    mergeFunction a b = snd a < snd b

    --Q2 Redefine map f and filter p using foldr
    q2map :: (a -> b) -> [a] -> [b]
    q2map f [] = []
    q2map f (xs) = foldr (\x xs -> f x : xs) [] xs

    q2function :: Int -> Int
    q2function a = (a + 3)

    q2filter :: (a -> Bool) -> [a] -> [a]
    q2filter p [] = []
    q2filter p (xs) = foldr (\x xs -> if p x then x : xs else xs) [] xs

    q2filterfunction :: Int -> Bool
    q2function a = (a + 3)

    