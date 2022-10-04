{-
  Week 2
  James March, 04/10/2022
-}


module Week2 where

    --Ceasar Cipher

    import Data.Char
    
    let2int :: Char -> Int
    let2int c = ord c - (ord 'a')

    int2let :: Int -> Char
    int2let n = chr (ord 'a' + n)

    shift :: Int -> Char -> Char
    shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
              | otherwise = c

    --Q1 Encode a string using Ceasar Cipher
    encode :: Int -> String -> String
    encode n xs = [(shift n x) | x <- xs]

    --Q2 Decode a string using Ceasar Cipher
    decode :: Int -> String -> String
    decode n xs = encode (-n) xs

    --Q3 Fibonacci using recursion
    fib :: Int -> Int
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)

    --List recursion example - find length of list
    len :: [a] -> Int
    len [] = 0
    len (x:xs) = 1 + len xs

    --Q4 Find nth value in a list
    nthItem :: Int -> [a] -> a
    nthItem n (x:xs) | (n == 1) = x
                     | otherwise = nthItem (n - 1) xs
                 
    --Q5 Find roots of a quadratic equation
    roots :: (Float, Float, Float) -> (Float, Float)
    roots (a, b, c) = (((-b + discriminant) / (2 * a)), ((-b - discriminant) / (2 * a)))
        where discriminant = sqrt ((b^2) - (4 * a * c))

    --Q6 Quick functions
    --a)stack takes the first element of a list and puts it on the end of a list
    stack :: [a] -> [a]
    stack (x:xs) = xs ++ [x]

    --b) range takes a numerical value and checks to see if it is between 0 and 10, returns True if it is False otherwise
    range :: Int -> Bool
    range n = (n >= 0 && n <= 10)

    --c) addc takes a Char and a String and adds the Char to the beginning of the String
    addc :: Char -> String -> String
    addc c xs = [c] ++ xs

    --d) halves takes a list and divides each element in the list by two
    halves :: [Float] -> [Float]
    halves xs = [(x / 2) | x <- xs]

    --e) capitalizeStart that takes a string as input and returns the same string with the first character capitalized. (If the first
    --character is not a lowercase letter, it should simply return the input string.)
    capitalizeStart :: String -> String
    capitalizeStart (x:xs) = [toUpper x] ++ xs

    --Q7 Map and filter example
    squareEvens :: [Int]
    squareEvens = map (^2) (filter even [1..20])

    --Q8 Tail as a lambda expression
    mystery8 = (\ (_:xs) -> xs)

    --Q9 Lambda functions
    --a) Increment an integer value
    increment :: Int -> Int
    increment n = (\x -> x + 1) n

    --b) decrement an integer value
    decrement :: Int -> Int
    decrement n = (\x -> x - 1) n
