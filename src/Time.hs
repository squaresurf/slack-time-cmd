module Time
  ( dbTimeZoneToTime
  )
where

import           Data.Function                  ( (&) )
import qualified Data.Time.Clock               as Clock
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import qualified Data.Time.LocalTime           as LocalTime
import qualified Db

type FormattedDate = String

dbTimeZoneToTime :: Db.TimeZone -> IO String
dbTimeZoneToTime (offset, label) = do
  timeString <- Time.getCurrentTimeWithOffset offset
  return $ concat [timeString, " : ", label]

getCurrentTimeWithOffset :: Int -> IO FormattedDate
getCurrentTimeWithOffset offsetSeconds =
  formatTime defaultTimeLocale "%r"
    .   LocalTime.utcToZonedTime tz
    <$> Clock.getCurrentTime
  where tz = LocalTime.minutesToTimeZone $ div offsetSeconds 60
