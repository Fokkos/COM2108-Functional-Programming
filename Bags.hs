module Bags where
    import Data.List

    type Item a = (a, Int)

    type Bag a = [Item a]

    listToBag :: Eq a => [a] -> Bag a
    listToBag (xs) = [(x, i) | x <- xs]
        where i = 1

    

{-
    data Bag a = Bag [a]

    listToBag :: [a] -> Bag a
    listToBag xs = Bag xs

    bagEqual :: (Eq a, Ord a) => Bag a -> Bag a -> Bool
    bagEqual (Bag bagA) (Bag bagB) | length bagA /= length bagB = False
                                   | bagA == [] && bagB == [] = True
                                   | elem (head (bagA)) (bagB) = bagEqual (Bag (tail (bagA))) (Bag (removeFromList [(head (bagA))] bagB))

    removeFromList :: Eq a => [a] -> [a] -> [a]
    removeFromList _ []           = []
    removeFromList (y:ys) (x:xs)
                    | y == x = xs
                    | otherwise = x : removeFromList [y] xs

    --bagEqual :: (Eq a) => Bag a -> [a]
    --bagEqual (Bag bagA) = tail bagA
-}
    