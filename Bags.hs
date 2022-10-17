module Bags where
    data Bag a = Bag [a]

    listToBag :: [a] -> Bag a
    listToBag xs = Bag xs

    --bagEqual :: Bag a -> Bag a -> Bool
    --bagEqual xs ys = ((length xs) == (length ys))
