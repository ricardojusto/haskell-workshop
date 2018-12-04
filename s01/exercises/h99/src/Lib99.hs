module Lib99 where

-- Last Element
problem1 :: [a] -> a
problem1 [] = error "last element undefined for empty list"
problem1 [x] = x
problem1 (x:xs) = problem1 xs

-- But Last Element
problem2 :: [a] -> a
problem2 = head . tail . reverse

-- k'th element
problem3_1 :: [a] -> Int -> a
problem3_1 [] _ = error "not enough elements"
problem3_1 (x:xs) p = if p==1 then x else problem3_1 xs (p - 1)

problem3_2 :: [a] -> Int -> a
problem3_2 [] _ = error "not enough elements"
problem3_2 (x:_)  1 = x
problem3_2 (x:xs) p = problem3_2 xs (p - 1)

problem3_3 :: [a] -> Int -> a
problem3_3 l p
    | length l == 0 = error "not enough elements"
    |        p == 1 = head l
    |     otherwise = problem3_3 (tail l) (p-1)

-- intFromTo
-- não dá sem o "Ord a", porquê?
-- qual a assinatura do enumFromTo?
fromto :: (Enum a, Ord a) => a -> a -> [a]
fromto m n
    | m <= n    = (m : (fromto (succ m) n))
    | otherwise = []

