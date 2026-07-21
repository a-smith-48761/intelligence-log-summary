import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec

import LogParser

testTree :: TestTree
testTree = testGroup "parser tests" 
    [
        testCase "can parse dates" $ do
            runParser parseDate () "test data" "12/11/1009" @?= Right (DateRecord 12 11 1009)
    ]

main :: IO ()
main = defaultMain testTree
