module Week4 where
    {-
    Q3
    sumwith :: Int -> [Int] -> Int
    sumwith v [] = v
    sumwith v (x:xs) = sumwith (v+x) xs

    sumwith 4 [1,2,0]
    sumwith (4 + 1) [2,0]
    sumwith (4 + 1 + 2) [0]
    sumwith (4 + 1 + 2 + 0) [] = (4 + 1 + 2 + 0) = 7
    -}