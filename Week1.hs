{-
  Week 1
  James March, 29/09/2022
-}

module Week1 where
    {-
        1)
			second = String
			swap = Tuple
			pair = Tuple
			double = Integer
			palin = Boolean
			twice = Integer
    -}

    --Q2, recreate Euclid algorithm
    euclid :: Integer -> Integer -> Integer -> Integer -> Float -- type definition
    euclid x1 x2 y1 y2 = sqrt ( fromIntegral (((x2 - x1)^2) +((y2 - y1)^2)))

    --Q3 Function to extract first word of a string
    firstWord :: String -> String
    firstWord xs = takeWhile (/= ' ') xs

    --Q4 Split a list into two smaller lists half the size
    halve :: [a] -> [[a]]
    halve xs = [(take ((length xs) `div` 2) xs), (drop ((length xs) `div` 2) xs)]

    --Guarded Equations example
    max2 :: Integer -> Integer -> Integer
    max2 a b | a > b     = a
             | otherwise = b

    --Q5 Work out average of 4 grades and give suitable mark
    grades :: Integer -> Integer -> Integer -> Integer -> String
    grades g1 g2 g3 g4 | mean >= 70 = "H1"
                       | mean >= 60 = "H2.1"
                       | mean >= 50 = "H2.2"
                       | mean >= 45 = "H3"
                       | mean >= 40 = "Pass"
                       | otherwise  = "Fail"
                           where mean = (g1 + g2 + g3 + g4) `div` 4

    --Q6 Get all odd numbers from a list of Integers
    oddItems :: [Int] -> [Int]
    oddItems xs = filter (odd) xs

    --Q7 Different list examples
    {- 
      [-1..1] Prints [-1,0,1]
      [0.1..1] Prints [0.1,1.1]
      [1.2,0.9..0] Prints [1.2,0.9,0.6000000000000001,0.30000000000000016,2.220446049250313e-16]
        (rounding error ig??)
      [10..1] Prints []
      [1..] Prints an infinite list lol
    -}

    --Q8 Same as Q5 but for a list of grades
    gradesList :: [Int] -> String
    gradesList xs | mean >= 70 = "H1"
                  | mean >= 60 = "H2.1"
                  | mean >= 50 = "H2.2"
                  | mean >= 45 = "H3"
                  | mean >= 40 = "Pass"
                  | otherwise  = "Fail"
                      where mean = (fromIntegral (sum xs) / fromIntegral (length xs))

    --Q9 = Challenge... Do later






