module Time
  ( getCurrentTimeWithOffset
  )
where

import           Data.Function                  ( (&) )
import qualified Data.Time.Clock               as Clock
import           Data.Time.Format               ( defaultTimeLocale
                                                , formatTime
                                                )
import qualified Data.Time.LocalTime           as LocalTime

type FormattedDate = String

getCurrentTimeWithOffset :: Int -> IO FormattedDate
getCurrentTimeWithOffset offsetSeconds =
  formatTime defaultTimeLocale "%r"
    .   LocalTime.utcToZonedTime tz
    <$> Clock.getCurrentTime
  where tz = LocalTime.minutesToTimeZone $ div offsetSeconds 60
