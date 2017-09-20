module Main where

import Daily (everything)
import Daily.IO (runDailyM)

main :: IO ()
main = runDailyM everything
