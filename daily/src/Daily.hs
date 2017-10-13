{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleContexts, OverloadedStrings, TemplateHaskell #-}

module Daily (DailyOp(..), DailyM(..), everything) where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

data DailyOp next = RunOSAScript String next
                  | MacOpen String next
                  | WriteMessage String next
                  | WriteMessageLn String next
                  deriving (Functor)

type DailyM = Free DailyOp

makeFree ''DailyOp

completeScript :: String
completeScript =
  "tell application \"Things3\"\n" ++
  "  repeat with toDo in to dos of list \"Today\"\n" ++
  "    set toDoName to name of toDo\n" ++
  "    if (toDoName as string) is equal to \"Run Daily\" then\n" ++
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

showBluetoothMenu :: DailyM ()
showBluetoothMenu =
  macOpen "/System/Library/CoreServices/Menu Extras/Bluetooth.menu"

everything :: DailyM ()
everything = do
  indicating "Showing bluetooth menu" $ showBluetoothMenu
  indicating "Checking off Daily Run task" $ runOSAScript completeScript
  indicating "Syncing Anki" $ runOSAScript ankiScript
