{-# LANGUAGE FlexibleContexts #-} 
module LogParser (
    DateRecord(DateRecord),
    TimeRecord(TimeRecord),
    LogRecord(ContactRecord,NonContactRecord),
    parseDate, parseTime, parseLine, parseLog
) where

import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Char
import Control.Monad (void)

data DateRecord = DateRecord Int Int Int deriving (Show, Read, Eq)
data TimeRecord = TimeRecord Int Int Int deriving (Show, Read, Eq)
data LogRecord =
    ContactRecord DateRecord TimeRecord String TimeRecord String String |
    NonContactRecord DateRecord TimeRecord TimeRecord String
    deriving (Show, Read, Eq)

countBetween :: Stream s m t => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
countBetween n limit parser = do
    prefix <- count n parser
    suffix <- manyWithLimit (limit - n) parser
    return $ prefix ++ suffix

-- from https://stackoverflow.com/questions/49751139/how-to-combine-parsers-up-to-n-times-in-haskell
manyWithLimit :: Int -> ParsecT s u m a -> ParsecT s u m [a]
manyWithLimit n p | n > 0 = (:) <$> try p <*> manyWithLimit (n-1) p <|> return []
manyWithLimit _ _ = return []

parseDate :: Stream s m Char => ParsecT s u m DateRecord
parseDate = do
    day <- countBetween 1 2 digit
    void $ char '/'
    month <- countBetween 1 2 digit
    void $ char '/'
    year <- count 4 digit
    return $ DateRecord (read day) (read month) (read year)

parseTime :: Stream s m Char => ParsecT s u m TimeRecord
parseTime = do
    hour <- countBetween 1 2 digit
    void $  char ':'
    mins <- count 2 digit
    void $  char ':'
    sec  <- count 2 digit
    return $ TimeRecord (read hour) (read mins) (read sec)

recordCharacter :: Stream s m Char => ParsecT s u m Char
recordCharacter = noneOf "\t\r\n"

parseLine :: Stream s m Char => ParsecT s u m LogRecord
parseLine = choice [try parseContactRecord, parseNonContactRecord]
    where
        parseContactRecord = do
            daterec <- parseDate
            void $ string ", "
            timerec <- parseTime
            void $ char '\t'
            recid <- many1 alphaNum
            void $ char '\t'
            duration <- parseTime
            void $ char '\t'
            status <- many1 recordCharacter
            void $ char '\t'
            note <- many1 $ noneOf "\r\n"
            return $ ContactRecord daterec timerec recid duration status note

        parseNonContactRecord = do
            daterec <- parseDate
            void $ string ", "
            timerec <- parseTime
            void $ count 2 $ char '\t'
            duration <- parseTime
            void $ char '\t'
            status <- many1 recordCharacter
            skipMany $ oneOf "\t " -- ignore terminal whitespace
            return $ NonContactRecord daterec timerec duration status

parseLog :: String -> IO String -> IO (Either ParseError [LogRecord])
parseLog name content = content >>= runParserT (sepBy parseLine endOfLine) () name
