{-# LANGUAGE FlexibleContexts #-} 
module LogParser (
    DateRecord(DateRecord),
    parseDate
) where

import Text.Parsec

data DateRecord = DateRecord Int Int Int deriving (Show, Read, Eq)

parseDate :: Stream s m Char => ParsecT s u m DateRecord
parseDate = do
    day <- many1 digit
    _ <- char '/'
    month <- many1 digit
    _ <- char '/'
    year <- many1 digit
    return $ DateRecord (read day) (read month) (read year)
