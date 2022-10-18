module Bags where
    import Data.List

    data Bag a = Bag [a]

    listToBag :: [a] -> Bag a
    listToBag xs = Bag xs

    bagEqual :: (Eq a, Ord a) => Bag a -> Bag a -> Bool
    bagEqual (Bag bagA) (Bag bagB) | length bagA /= length bagB = False
                                   | bagA == [] && bagB == [] = True
                                   | elem (head (sort bagA)) (sort bagB) = bagEqual (listToBag (tail (sort bagA))) (listToBag (tail (sort bagB)))


    --bagEqual :: (Eq a) => Bag a -> [a]
    --bagEqual (Bag bagA) = tail bagA