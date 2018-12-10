import Data.List

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Lib99

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Problems" 
    [ problem1Spec
    , problem2Spec
    , problem3Spec
    , problem4Spec
    , problem5Spec
    , problem6Spec
    , problem7Spec
    , problem8Spec
    , problem9Spec
    , problem10Spec
    ]

problem1Spec :: TestTree
problem1Spec = 
     testGroup "Problem #1" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a singleton list return its last element" $
            (myLast [1]) @?= (1 :: Int)

        , testCase "Given a list return its last element" $
            (myLast [1, 2, 3]) @?= (3 :: Int)
        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "head == (myLast . reversed)" prop_transformationEquivalence
        , QC.testProperty "myLast == last" prop_preludeEquivalence
        ]
     ]
         where prop_transformationEquivalence :: NonEmptyList Int -> Bool
               prop_transformationEquivalence (NonEmpty xs) = 
                   (head xs) == (myLast (reverse xs))

               prop_preludeEquivalence :: NonEmptyList Int -> Bool
               prop_preludeEquivalence (NonEmpty xs) =
                   (last xs) == (myLast xs)


problem2Spec :: TestTree
problem2Spec = 
     testGroup "Problem #2" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a list with two elements return its first element" $
            (myButLast [1, 2]) @?= (1 :: Int)

        , testCase "Given a list return its second to last element" $
            (myButLast [1, 2, 3]) @?= (2 :: Int)
        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "myButLast == (last . last)" prop_preludeEquivelence 
        ]
     ]
         where prop_preludeEquivelence :: NonEmptyList Int -> Property
               prop_preludeEquivelence (NonEmpty xs) = 
                   (not . null) (drop 2 xs) ==>
                       ((last . init) xs) == (myButLast xs)


problem3Spec :: TestTree
problem3Spec = 
     testGroup "Problem #3" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a list and an index return the correspondent element" $
          (elementAt [1, 2, 3] 2) @?= (2 :: Int)

        , testCase "Given a list and an index return the correspondent element" $
          (elementAt "haskell" 5) @?= 'e'

        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "elementAt == (!!)" prop_preludeEquivelence 
        ]
     ]
         where prop_preludeEquivelence :: NonEmptyList Int -> Property
               prop_preludeEquivelence (NonEmpty xs) = 
                   forAll (choose (0, length xs - 1)) $ \n -> 
                       (xs !! n) == (elementAt xs (n + 1))


problem4Spec :: TestTree
problem4Spec = 
     testGroup "Problem #4" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a list return its length" $
          (myLength ([123, 456, 789] :: [Int])) @?= (3 :: Int)

        , testCase "Given a list return length" $
          (myLength "Hello, world!") @?= (13 :: Int)

        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "myLength == length" prop_preludeEquivelence 
        ]
     ]
         where prop_preludeEquivelence :: [Int] -> Bool
               prop_preludeEquivelence xs = length xs == myLength xs


problem5Spec :: TestTree
problem5Spec = 
     testGroup "Problem #5" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a list return its reverse" $
          (myReverse "A man, a plan, a canal, panama!") @?= "!amanap ,lanac a ,nalp a ,nam A"

        , testCase "Given a list return its reverse" $
          (myReverse ([1, 2, 3, 4] :: [Int])) @?= ([4, 3, 2, 1] :: [Int])

        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "myReverse == reverse" prop_preludeEquivelence 
        ]
     ]
         where prop_preludeEquivelence :: [Int] -> Bool
               prop_preludeEquivelence xs = reverse xs == myReverse xs


problem6Spec :: TestTree
problem6Spec = 
     testGroup "Problem #6" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a range return false" $
          (isPalindrome ([1,2,3] :: [Int])) @?= False

        , testCase "Given a palindrome return true" $
          (isPalindrome "madamimadam") @?= True

        , testCase "Given a palindrome return true" $
          (isPalindrome ([1,2,4,8,16,8,4,2,1] :: [Int])) @?= True
        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "myReverse == reverse" prop_preludeEquivelence 
        ]
     ]
         where prop_preludeEquivelence :: [Int] -> Bool
               prop_preludeEquivelence xs = isPalindrome xs == (xs == reverse xs)


problem7Spec :: TestTree
problem7Spec = 
     testGroup "Problem #7" 
     [ testGroup "(checked by HUnit)"
      [ testCase "Given a flat nestedList return a list" $
        (flatten ((Elem 5) :: NestedList Int)) @?= ([5] :: [Int])
      , testCase "Given a palindrome return true" $
        (flatten ((List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) :: NestedList Int)) @?= ([1, 2, 3, 4, 5] :: [Int])
      , testCase "Given a palindrome return true" $
        (flatten ((List []) :: NestedList Int)) @?= ([] :: [Int])
      ]
     ]


problem8Spec :: TestTree
problem8Spec = 
     testGroup "Problem #8" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a sequence return the same sequence" $
          (compress ([1 .. 20] :: [Int])) @?= ([1 .. 20] :: [Int])

        , testCase "Given a string compress it" $
          (compress "aaaabccaadeeee") @?= "abcade"
        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "length >= length . compress" prop_length
        , QC.testProperty "intersect compressed xs == compressed" prop_elements
        ]
     ]
         where prop_length :: [Int] -> Bool
               prop_length xs = length xs >= (length . compress) xs

               prop_elements :: [Int] -> Bool
               prop_elements xs = intersect (compress xs) xs == (compress xs)


problem9Spec :: TestTree
problem9Spec = 
     testGroup "Problem #9" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a list with a single element return a list with that list within" $
          (pack ['a']) @?= [['a']]
        , testCase "Given a packable list, pack it" $
          (pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']) @?= (["aaaa","b","cc","aa","d","eeee"])
        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "length >= length . pack" prop_length
        , QC.testProperty "intersect compressed xs == compressed" prop_elements
        ]
     ]
         where prop_length :: [Int] -> Bool
               prop_length xs = length xs >= (length . pack) xs

               prop_elements :: [Int] -> Bool
               prop_elements xs = concat (pack xs) == xs



problem10Spec :: TestTree
problem10Spec = 
     testGroup "Problem #10" 
     [ testGroup "(checked by HUnit)"
        [ testCase "Given a list with many elements return RLE " $
          (encode "aaaabccaadeeee") @?= ([(4, 'a'),(1, 'b'),(2, 'c'),(2, 'a'),(1, 'd'),(4, 'e')] :: [(Int, Char)])
        ],

      testGroup "(checked by QuickCheck)"
        [ QC.testProperty "length >= length . compress" prop_length
        , QC.testProperty "intersect compressed xs == compressed" prop_elements
        ]
     ]
         where prop_length :: [Int] -> Bool
               prop_length xs = (length xs) == ((sum . (map fst) . encode) xs)

               prop_elements :: [Int] -> Bool
               prop_elements xs = (intersect (map snd (encode xs)) xs) == (map snd (encode xs))
{-|
>>> 

-}

