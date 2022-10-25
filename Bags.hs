module Bags where
    type Bag a = [(Int, a)]

    listToBag :: Eq a => [a] -> Bag a
    listToBag [] = []
    listToBag (x:xs) = bagInsert x (listToBag xs)

    bagEqual :: Eq a => Bag a -> Bag a -> Bool
    bagEqual bag1 bag2 = length bag1 == length bag2 && and [ n1 == n2 | (n1, i1) <- bag1, (n2, i2) <- bag2, i1 == i2 ]

    bagInsert :: Eq a => a -> Bag a -> Bag a
    bagInsert item [] = [(1,item)]
    bagInsert item ((n,i):rest) | i == item = (n+1, i):rest
                                | otherwise = (n, i):(bagInsert item rest)

    bagSum :: Eq a => Bag a -> Bag a -> Bag a
    bagSum [] bag2 = bag2
    bagSum bag1 [] = bag1
    bagSum ((n,i):rest) bag2 = bagSum rest (bagMultipleInsert (n,i) bag2)

    bagMultipleInsert :: Eq a => (Int, a) -> Bag a -> Bag a
    bagMultipleInsert (n, i) [] = [(n, i)]
    bagMultipleInsert (n, i) ((n2,i2):rest) | i2 == i = (n + n2, i):rest
                                            | otherwise = (n2, i2):(bagMultipleInsert (n, i) rest)

    bagIntersection :: Eq a => Bag a -> Bag a -> Bag a
    bagIntersection b1 b2 = [(min n1 n2, i1) | (n1, i1) <- b1, (n2, i2) <- b2, i1 == i2]

   
