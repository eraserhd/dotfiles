{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, OverloadedStrings, TemplateHaskell #-}

module Daily (REST(..), DailyOp(..), DailyM(..), everything) where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Data.Aeson
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

data REST = POST { restUrl      :: String
                 , restBody     :: B.ByteString
                 , restUser     :: String
                 , restPassword :: String
                 }
          | GET  { restUrl      :: String
                 , restUser     :: String
                 , restPassowrd :: String
                 }
          deriving (Eq, Show)


data DailyOp next = CurrentTimeZone (TimeZone -> next)
                  | CurrentUTCTime (UTCTime -> next)
                  | GetEnv String (String -> next)
                  | RunOSAScript String next
                  | WriteMessage String next
                  | WriteMessageLn String next
                  | DoREST REST (Int -> next)
                  deriving (Functor)

type DailyM = Free DailyOp

makeFree ''DailyOp

centralParkProject :: Integer
centralParkProject = 38030885

data TimeEntry =
  TimeEntry { description  :: String
            , tags         :: [String]
            , duration     :: Integer
            , start        :: UTCTime
            , pid          :: Integer
            , created_with :: String
            }
  deriving (Generic, Show)

data CreateTimeEntry =
  CreateTimeEntry { time_entry :: TimeEntry
                  }
  deriving (Generic, Show)

instance ToJSON TimeEntry where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON CreateTimeEntry where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TimeEntry
instance FromJSON CreateTimeEntry

today :: TimeZone -> UTCTime -> Day
today tz now = localDay $ utcToLocalTime tz now

hourToday :: TimeZone -> UTCTime -> Int -> UTCTime
hourToday tz now hour =
  localTimeToUTC tz $ LocalTime (today tz now) (TimeOfDay 9 0 0)

isWeekDay :: TimeZone -> UTCTime -> Bool
isWeekDay tz now = let (_, _, dayOfWeek) = toWeekDate (today tz now) in
                   dayOfWeek < 6

timeEntryForToday :: TimeZone -> UTCTime -> TimeEntry
timeEntryForToday tz now =
  TimeEntry { description = ""
            , tags = []
            , duration = 8 * 60 * 60
            , start = hourToday tz now 9
            , pid = centralParkProject
            , created_with = "curl"
            }

addTimeToToggl :: DailyM Bool
addTimeToToggl = do
  now <- currentUTCTime
  tz <- currentTimeZone
  if isWeekDay tz now
  then do
    token <- getEnv "TOGGL_API_TOKEN"
    statusCode <- doREST $ POST { restUrl      = "https://www.toggl.com/api/v8/time_entries"
                                , restBody     = encode (CreateTimeEntry { time_entry = timeEntryForToday tz now })
                                , restUser     = token
                                , restPassword = "api_token"
                                }
    if (statusCode /= 200)
    then do writeMessageLn "Non-200 response from Toggl"
            return False
    else return True
  else return False

completeScript :: String
completeScript =
  "tell application \"Things3\"\n" ++
  "  repeat with toDo in to dos of list \"Today\"\n" ++
  "    set toDoName to name of toDo\n" ++
  "    if (toDoName as string) is equal to \"Add time to Toggl\" then\n" ++
  "      set completion date of toDo to current date\n" ++
  "    end if\n" ++
  "  end repeat\n" ++
  "end tell"

ankiScript :: String
ankiScript =
  "tell application \"Anki\"\n" ++
  "  activate\n" ++
  "end tell\n" ++
  "tell application \"System Events\"\n" ++
  "  keystroke \"y\"\n" ++
  "  delay 3\n" ++
  "  tell process \"Anki\"\n" ++
  "    tell menu bar 1\n" ++
  "      tell menu \"Tools\"\n" ++
  "        click menu item \"Check Media...\"\n" ++
  "      end tell\n" ++
  "    end tell\n" ++
  "  end tell\n" ++
  "  delay 3\n" ++
  "  keystroke \"y\"\n" ++
  "end tell\n"

indicating :: String -> DailyM a -> DailyM a
indicating msg action = do
  writeMessage (msg ++ "... ")
  result <- action
  writeMessageLn "ok"
  return result

everything :: DailyM ()
everything = do
  indicating "Adding time to Toggl" addTimeToToggl
  indicating "Checking off Toggl task" $ runOSAScript completeScript
  indicating "Syncing Anki" $ runOSAScript ankiScript
