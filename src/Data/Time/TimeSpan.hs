{-# LANGUAGE BangPatterns #-}
module Data.Time.TimeSpan
  ( TimeSpan
  , milliseconds, seconds, minutes, hours, days
  , toMilliseconds, toSeconds, toMinutes, toHours, toDays
  , diffUTCTimeTS, addUTCTimeTS
  , sleepTS
  , timeAction
  )
where

import Control.Concurrent
import Data.Time
import System.CPUTime

-- | An abstract timespan. Use the provided smart constructors to create
-- a meaningful timespan
newtype TimeSpan
  = TimeSpan { unTimeSpan :: Double } -- as milliseconds
  deriving (Eq, Ord)

instance Monoid TimeSpan where
    mempty = TimeSpan 0
    mappend (TimeSpan a) (TimeSpan b) = TimeSpan (a + b)

milliseconds :: Double -> TimeSpan
milliseconds = TimeSpan

seconds :: Double -> TimeSpan
seconds = milliseconds . (* 1000)

minutes :: Double -> TimeSpan
minutes = seconds . (* 60)

hours :: Double -> TimeSpan
hours = minutes . (* 60)

days :: Double -> TimeSpan
days = hours . (* 24)

toMilliseconds :: TimeSpan -> Double
toMilliseconds = unTimeSpan

toSeconds :: TimeSpan -> Double
toSeconds = (/1000) . toMilliseconds

toMinutes :: TimeSpan -> Double
toMinutes = (/60) . toSeconds

toHours :: TimeSpan -> Double
toHours = (/60) . toMinutes

toDays :: TimeSpan -> Double
toDays = (/24) . toHours

diffUTCTimeTS :: UTCTime -> UTCTime -> TimeSpan
diffUTCTimeTS a b = seconds $ fromRational $ toRational $ diffUTCTime a b

addUTCTimeTS :: TimeSpan -> UTCTime -> UTCTime
addUTCTimeTS a b = addUTCTime (fromRational $ toRational $ toSeconds a) b

sleepTS :: TimeSpan -> IO ()
sleepTS ts = threadDelay (round $ toMilliseconds ts * 1000)

timeAction :: IO a -> IO (TimeSpan, a)
timeAction action =
    do !t1 <- getCPUTime
       !a <- action
       !t2 <- getCPUTime
       let secs = fromIntegral (t2-t1) * 1e-12
       return (seconds secs, a)
