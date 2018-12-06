import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Lib99

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Problems" [problem1Spec]

problem1Spec :: TestTree
problem1Spec = testGroup "Problem #1" [qcProps, unitTests]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "head == (last . reversed)" prop_lastOfReversedIsHead
  ]

unitTests :: TestTree
unitTests = testGroup "(checked by HUnit)"
  [ testCase "Given a list return its last element" $
      (problem1 [1]) @?= (1 :: Int)

  -- the following test does not hold
  , testCase "Given a singleton list return its element" $
      (problem1 [1, 2, 3]) @?= (3 :: Int)
  ]


prop_lastOfReversedIsHead :: NonEmptyList Int -> Bool
prop_lastOfReversedIsHead (NonEmpty xs) =
    (head xs) == (problem1 (reverse xs))
