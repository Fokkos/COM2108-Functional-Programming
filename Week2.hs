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