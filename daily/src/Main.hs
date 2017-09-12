{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Aeson
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import GHC.Generics
import Network.HTTP.Simple
import System.Environment (getEnv)
import System.Process.Typed (runProcess_, proc, setStdin, byteStringInput)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B

centralParkProject :: Integer
centralParkProject = 38030885

data TimeEntry =
  TimeEntry { description :: String
            , tags :: [String]
            , duration :: Integer
            , start :: UTCTime
            , pid :: Integer
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

makeRequest :: CreateTimeEntry -> String -> IO Request
makeRequest body token = do
  req <- parseRequest "https://www.toggl.com/api/v8/time_entries"
  return $
    setRequestMethod "POST" $
    addRequestHeader "Content-Type" "application/json" $
    setRequestBasicAuth (B.pack token) "api_token" $
    setRequestBodyJSON body $
    req

addTimeToToggl :: IO ()
addTimeToToggl = do
  now <- getCurrentTime
  tz <- getTimeZone now
  when (isWeekDay tz now) $ do
    token <- getEnv "TOGGL_API_TOKEN"
    request <- makeRequest (CreateTimeEntry { time_entry = timeEntryForToday tz now }) token
    response <- httpNoBody request
    when (getResponseStatusCode response /= 200) $
      error "Non-200 response from Toggl"

completeScript :: L8.ByteString
completeScript = L8.pack $
  "tell application \"Things3\"\n" ++
  "  repeat with toDo in to dos of list \"Today\"\n" ++
  "    set toDoName to name of toDo\n" ++
  "    if (toDoName as string) is equal to \"Add time to Toggl\" then\n" ++
  "      set completion date of toDo to current date\n" ++
  "    end if\n" ++
  "  end repeat\n" ++
  "end tell"

ankiScript :: L8.ByteString
ankiScript = L8.pack $
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

runAppleScript :: L8.ByteString -> IO ()
runAppleScript script =
  runProcess_ $
    setStdin (byteStringInput script) $
    proc "osascript" ["-"]


main :: IO ()
main = do addTimeToToggl
          runAppleScript completeScript
          runAppleScript ankiScript
