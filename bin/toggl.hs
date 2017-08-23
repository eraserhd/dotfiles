#!/usr/bin/env stack
-- stack --resolver lts-9.1 --install-ghc runghc --package aeson --package bytestring --package http-conduit --package time --package typed-process
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Aeson
import Data.Time
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

hourToday :: TimeZone -> UTCTime -> Int -> UTCTime
hourToday tz now hour =
  let today = localDay (utcToLocalTime tz now) in
  localTimeToUTC tz (LocalTime today (TimeOfDay 9 0 0))

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

completeThingsTask :: IO ()
completeThingsTask =
  runProcess_ $
    setStdin (byteStringInput completeScript) $
    proc "osascript" ["-"]


main :: IO ()
main = do now <- getCurrentTime
          tz <- getTimeZone now
          token <- getEnv "TOGGL_API_TOKEN"
          request <- makeRequest (CreateTimeEntry { time_entry = timeEntryForToday tz now }) token
          response <- httpNoBody request
          when (getResponseStatusCode response /= 200) $
            error "Non-200 response from Toggl"
          completeThingsTask
