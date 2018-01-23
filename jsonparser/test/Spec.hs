import Test.Tasty
import Test.Tasty.HUnit

import Json (run, signedNumber)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Tests" [signedNumberTest]

signedNumberTest = testGroup "signedNumber"
  [ testCase "-123 -> Just (-123, \"\")" $
    run signedNumber "-123" @?= Just(-123, "")

  , testCase "-123abc -> Just (-123, \"abc\")" $
    run signedNumber "-123abc" @?= Just(-123, "abc")

  , testCase "+123 -> Just (123, \"\")" $
    run signedNumber "+123" @?= Just(123, "")

  , testCase "123 -> Nothing" $
    run signedNumber "123" @?= Nothing
  ]

