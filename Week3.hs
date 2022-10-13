{-
  Week 3
  James March, 11/10/2022
-}


module Week3 where
    import Data.List
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
    q2filterfunction a = (a `mod` 2 == 0)

    --Q3 working with custom type Shape
    data Shape = Circle Float | Rect Float Float deriving Show

    area :: Shape -> Float
    area (Circle r) = pi * r^2
    area (Rect l w) = l * w

    scale :: Float -> Shape -> Shape
    scale x (Circle r) = Circle (r * x)
    scale x (Rect l w) = Rect (l * x) (w * x)

    --Lecture 10
    mystery10 = sum . takeWhile (<10000) . filter odd . map (^2)
    --The mystery function takes a list of Ints, squares them, filters so only
    --odd numbers appear, removes all numbers greater than 10000 then sums

    --First Past the Post
    type Votes = [String]

    candidates :: Votes -> [String]
    candidates [] = []
    candidates (x:xs) = x : filter (/= x) (candidates xs)

    count2 :: String -> Votes -> Int
    count2 x = length . filter (==x)

    result :: Votes -> [(Int, String)]
    result votes = sort [(count2 v votes, v) | v <- candidates votes]

    winner :: Votes -> String
    winner = snd . last . result

    testVotes = ["Red", "Blue", "Green", "Red", "Blue", "Red"]

    --Q4 Rewrite the following expressions as list comprehensions:
    --a) map (+3) xs
    q4a :: [Integer] -> [Integer]
    q4alist = [1..5]
    q4a xs = foldr (\x xs -> x + 3 : xs) [] xs

    --filter (>7) xs
    q4b :: [Integer] -> [Integer]
    q4blist = [1..10]
    q4b = foldr (\x xs -> if x < 7 then x : xs else xs) []

