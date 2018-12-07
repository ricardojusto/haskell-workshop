module Lib99 where

{-|
Finds the last element of a list. 

>>> myLast [1,2,3,4]
4
>>> myLast ['x','y','z']
'z'

-}
myLast :: [a] -> a
myLast [] = error "last element undefined for empty list"
myLast [x] = x
myLast (x:xs) = myLast xs


{-|
Find the last but one element of a list.
       
>>> myButLast [1,2,3,4]
3
>>> myButLast ['a'..'z']
'y'
-}
myButLast :: [a] -> a
myButLast [] = error "but last element undefined for empty list"
myButLast [x] = error "but last element undefined for singleton list"
myButLast [x, y] = x
myButLast (_:xs) = myButLast xs

{-|
Find the K'th element of a list. The first element in the list is number 1.

>>> elementAt [1,2,3] 2
2
>>> elementAt "haskell" 5
'e'
-}
elementAt :: [a] -> Int -> a
elementAt [] _ = error "not enough elements"
elementAt (x:_)  1 = x
elementAt (x:xs) p = elementAt xs (p - 1)

{-|
Find the number of elements of a list. 

>>> myLength [123, 456, 789]
3
>>> myLength "Hello, world!"
13
-}
myLength :: [a] -> Int
myLength = undefined


{-|
Reverse a list. 

>>> myReverse "A man, a plan, a canal, panama!"
"!amanap ,lanac a ,nalp a ,nam A"
>>> myReverse [1,2,3,4]
[4,3,2,1]
-}
myReverse :: [a] -> [a]
myReverse = undefined


{-|
Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

>>> isPalindrome [1,2,3]
False
>>> isPalindrome "madamimadam"
True
>>> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = undefined


-- |We have to define a new data type, because lists in Haskell are homogeneous. 
data NestedList a = Elem a | List [NestedList a]

{-|
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

>>> flatten (Elem 5)
[5]
>>> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
>>> flatten (List [])
[]
-}
flatten :: NestedList a -> [a]
flatten = undefined


{-|
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed. 

>>> compress "aaaabccaadeeee"
"abcade"
-}
compress :: (Eq a) => [a] -> [a]
compress = undefined

{-|
Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists. 

>>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
["aaaa","b","cc","aa","d","eeee"]
-}
pack :: (Eq a) => [a] -> [[a]]
pack = undefined

{-|
Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E. 

>>> encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
encode :: (Eq a) => [a] -> [(Int, a)]
encode = undefined
