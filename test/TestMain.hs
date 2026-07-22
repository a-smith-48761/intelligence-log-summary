import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec

import LogParser

maybeRight :: Either a b -> Maybe b
maybeRight (Left _) = Nothing
maybeRight (Right r) = Just r
assertParseError :: (Eq b, Show b) => Either a b -> Assertion
assertParseError res = maybeRight res @?= Nothing

testFileData :: IO String
testFileData = return $
                "22/07/2026, 09:09:09\t129xxx1\t00:00:23\tRefused\txxx/xxxxx\n" ++
                "22/07/2026, 09:09:09\t\t00:00:23\tInterviewer_Wait\t "
            
testTree :: TestTree
testTree = testGroup "parser tests" 
    [
        testCase "can parse dates" $ do
            runParser parseDate () "test data" "12/11/1009" 
                @?= Right (DateRecord 12 11 1009),
        testCase "date parse failure cases" $ do
            assertParseError $ runParser parseDate () "test data" "xx/xx/xxxx"
            assertParseError $ runParser parseDate () "test data" "01-23-4567"
            assertParseError $ runParser parseDate () "test data" "123/45/6789",

        testCase "can parse times" $ do
            runParser parseTime () "test data" "12:11:10" 
                @?= Right (TimeRecord 12 11 10),

        testCase "can parse line with record id and notes" $ do
            runParser parseLine () "test data" "22/07/2026, 09:09:09\t129xxx1\t00:00:23\tRefused\txxx/xxxxx"
                @?= Right (ContactRecord 
                    (DateRecord 22 7 2026) 
                    (TimeRecord 9 9 9)
                    "129xxx1"
                    (TimeRecord 0 0 23)
                    "Refused"
                    "xxx/xxxxx"),

        testCase "can parse line without record id and notes" $ do
            runParser parseLine () "test data" "22/07/2026, 09:09:09\t\t00:00:23\tInterviewer_Wait\t "
                @?= Right (NonContactRecord 
                    (DateRecord 22 7 2026) 
                    (TimeRecord 9 9 9)
                    (TimeRecord 0 0 23)
                    "Interviewer_Wait"),

        testCase "parser works on IO-strings" $ do
            logdata <- parseLog "stdin" testFileData 
            logdata @?= 
                Right [
                    (ContactRecord 
                        (DateRecord 22 7 2026) 
                        (TimeRecord 9 9 9)
                        "129xxx1"
                        (TimeRecord 0 0 23)
                        "Refused"
                        "xxx/xxxxx"),
                     (NonContactRecord 
                        (DateRecord 22 7 2026) 
                        (TimeRecord 9 9 9)
                        (TimeRecord 0 0 23)
                        "Interviewer_Wait")
                ]
    ]

main :: IO ()
main = defaultMain testTree
