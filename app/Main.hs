module Main where

import LogParser
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.List
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Char(toUpper)
import Text.Parsec.Error(ParseError)

data StatusCategory =
  AnsweringMachine | CallbackAnotherTime | Appointment |
  Refusal | Abandoned | Complete | NoAnswer | OOQ |
  HUDI_CBAT | HUDI_Ref | Screened | Waiting | Other
  deriving (Show, Eq, Ord)

data StatCounter =
  StatCounter {
    ss_count :: Int,
    ss_totalSecs :: Int,
    ss_totalSquareSecs ::Int
  }
  deriving (Show, Eq, Ord)

data Summary =  Summary
  (Map StatusCategory StatCounter)  -- summaries for each kind of item we're tracking
  StatCounter                       -- and overall summary of all items
  (Set String)                      -- and set of outcomes included in the "Other" category
  deriving (Show, Eq, Ord)

recordDurationInSeconds :: LogRecord -> Int
recordDurationInSeconds (ContactRecord _ _ _ duration _ _) = durationToSeconds duration
recordDurationInSeconds (NonContactRecord _ _ duration _)  = durationToSeconds duration

recordOutcome :: LogRecord -> String
recordOutcome (ContactRecord _ _ _ _ outcome _) = outcome
recordOutcome (NonContactRecord _ _ _ outcome)  = outcome

durationToSeconds :: TimeRecord -> Int
durationToSeconds (TimeRecord h m s) = h*3600 + m*60 + s

{-
# Special cases:
# * An outcome of "Interviewer_Wait" is time spent waiting for a call to be connected
# * We identify subcases of outcomes based on presence of specific markers in the notes:
#   * "HUDI" is a subcase of both the CallbackAnotherTime and Refused categories, which we count separately.
#   * "Screening" is also a subcase of CallbackAnotherTime that's tracked separately.
# Outcome categories we track are: `Answering Machine, CallbackAnotherTime, Appointment*, *Refus*, Abandon*, Complet*, NoAnswer/Busy, OOQ*`.
# Any other categories are reported in an "Other" category along with a brief list.
-}

recordCategory :: LogRecord -> StatusCategory
recordCategory (ContactRecord _ _ _ _ cat note)
  | cat == "Answering machine"                                     = AnsweringMachine
  | "HUDI" `isInfixOf` ucaseNote && cat == "CallbackAnotherTime"   = HUDI_CBAT
  | "HUDI" `isInfixOf` ucaseNote && "Refus" `isPrefixOf` cat       = HUDI_CBAT
  | "SCREEN" `isInfixOf` ucaseNote                                 = Screened
  | cat == "CallbackAnotherTime"                                   = CallbackAnotherTime
  | "Refus" `isInfixOf` cat                                        = Refusal
  | "Appoint" `isPrefixOf` cat                                     = Appointment
  | "Abandon" `isPrefixOf` cat                                     = Abandoned
  | "Complet" `isPrefixOf` cat                                     = Complete
  | "OOQ" `isPrefixOf` cat                                         = OOQ
  | otherwise                                                      = Other
  where
      ucaseNote = toUpper <$> note
recordCategory (NonContactRecord {}) = Waiting -- this seems to be the only cat used for non contact

emptyStats :: StatCounter
emptyStats = StatCounter 0 0 0

emptySummary :: Summary
emptySummary = Summary Map.empty emptyStats Set.empty

updateSummary :: LogRecord -> Summary -> Summary
updateSummary record (Summary catMap totals otherOutcomes) =
  let duration = recordDurationInSeconds record
      category = recordCategory record
      in
        Summary (updateCategoryMap catMap category duration)
                (updateStats totals duration)
                (updateOtherSet otherOutcomes category $ recordOutcome record)
  where
    -- updateCategoryMap adds duration to the stats for category cat, or adds a new stat counter if none exists
    updateCategoryMap m cat duration = Map.alter (alterStats duration) cat m

    -- updateStats adds duration to the stat counter given
    updateStats (StatCounter count sumDuration sumSqDuration) duration =
      StatCounter (count + 1) (sumDuration + duration) (sumSqDuration + duration*duration)

    -- alterStats is used for map updates: if the stats argument is nothing we add duration to an empty stat counter
    -- otherwise we update the existing counter. We never return "Nothing" because that would delete the entry from
    -- the map
    alterStats duration Nothing  = Just $ updateStats emptyStats duration
    alterStats duration (Just stats)  = Just $ updateStats stats duration

    -- update the Other outcomes set: we add the outcome if the category is Other but otherwise do nothing
    updateOtherSet s Other outcome = Set.insert outcome s
    updateOtherSet s _ _ = s

summaryToText :: Summary -> String
summaryToText = show

generateLogSummary :: Either ParseError [LogRecord] -> String
generateLogSummary (Left msg) = "Error parsing log: " ++ show msg
generateLogSummary (Right recs) = summaryToText $ foldr updateSummary emptySummary recs

main :: IO ()
main =  parseLog "stdin" getContents >>= putStrLn . generateLogSummary
