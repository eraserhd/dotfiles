module Daily.IO (runDailyM) where

import Daily

import Control.Monad.Free (iterM)
import Data.Time
import Network.HTTP.Simple
import System.Environment (getEnv)
import System.IO (hPutStr, hFlush, stdout)
import System.Process.Typed (runProcess_, proc)

dailyOpInterpret                            :: DailyOp (IO a) -> IO a
dailyOpInterpret (CurrentTimeZone next)     = getCurrentTime >>= getTimeZone >>= next
dailyOpInterpret (CurrentUTCTime next)      = getCurrentTime >>= next
dailyOpInterpret (GetEnv name next)         = getEnv name >>= next
dailyOpInterpret (RunOSAScript script next) = runProcess_ (proc "osascript" ["-e", script]) >> next
dailyOpInterpret (WriteMessage msg next)    = hPutStr stdout msg >> hFlush stdout >> next
dailyOpInterpret (WriteMessageLn msg next)  = putStrLn msg >> next
dailyOpInterpret (DoREST url reqfn next)    = do
  request <- reqfn <$> parseRequest url
  response <- httpNoBody request
  next response

runDailyM :: DailyM a -> IO a
runDailyM = iterM dailyOpInterpret
