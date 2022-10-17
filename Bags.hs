module Bags where
    data Bag a = Bag [a]

    listToBag :: [a] -> Bag a
    listToBag xs = Bag xs

