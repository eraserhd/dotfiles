{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, GADTs, OverloadedStrings, TemplateHaskell #-}

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Data.Aeson
import Data.Time
import Data.Time.Calendar.WeekDate (toWeekDate)
import GHC.Generics
import Network.HTTP.Simple
import qualified System.Environment as E (getEnv)
import System.IO (hPutStr, hFlush, stdout)
import System.Process.Typed (runProcess_, proc)
import qualified Data.ByteString.Char8 as B

data DailyOp next = CurrentTimeZone (TimeZone -> next)
                  | CurrentUTCTime (UTCTime -> next)
                  | GetEnv String (String -> next)
                  | RunOSAScript String next
                  | WriteMessage String next
                  | WriteMessageLn String next
                  | DoREST String (Request -> Request) (Response () -> next)
                  deriving (Functor)

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

addTimeToToggl :: Free DailyOp Bool
addTimeToToggl = do
  now <- currentUTCTime
  tz <- currentTimeZone
  if isWeekDay tz now
  then do
    token <- getEnv "TOGGL_API_TOKEN"
    response <- doREST "https://www.toggl.com/api/v8/time_entries" $
                  setRequestMethod "POST" .
                  setRequestHeader "Content-Type" [B.pack "application/json"] .
                  setRequestBasicAuth (B.pack token) "api_token" .
                  setRequestBodyJSON (CreateTimeEntry { time_entry = timeEntryForToday tz now })
    if (getResponseStatusCode response /= 200)
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

indicating :: String -> Free DailyOp a -> Free DailyOp a
indicating msg action = do
  writeMessage (msg ++ "... ")
  result <- action
  writeMessageLn "ok"
  return result

everything :: Free DailyOp ()
everything = do
  indicating "Adding time to Toggl" addTimeToToggl
  indicating "Checking off Toggl task" $ runOSAScript completeScript
  indicating "Syncing Anki" $ runOSAScript ankiScript

dailyOpInterpret                            :: DailyOp (IO a) -> IO a
dailyOpInterpret (CurrentTimeZone next)     = getCurrentTime >>= getTimeZone >>= next
dailyOpInterpret (CurrentUTCTime next)      = getCurrentTime >>= next
dailyOpInterpret (GetEnv name next)         = E.getEnv name >>= next
dailyOpInterpret (RunOSAScript script next) = runProcess_ (proc "osascript" ["-e", script]) >> next
dailyOpInterpret (WriteMessage msg next)    = hPutStr stdout msg >> hFlush stdout >> next
dailyOpInterpret (WriteMessageLn msg next)  = putStrLn msg >> next
dailyOpInterpret (DoREST url reqfn next)    = do
  request <- reqfn <$> parseRequest url
  response <- httpNoBody request
  next response

main :: IO ()
main = iterM dailyOpInterpret everything
