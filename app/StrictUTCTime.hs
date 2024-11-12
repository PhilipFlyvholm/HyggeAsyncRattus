{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module StrictUTCTime where

import Data.Time.Clock
    ( getCurrentTime, DiffTime, UTCTime(UTCTime), diffUTCTime, NominalDiffTime, addUTCTime )
import Data.Time.Calendar ( Day )
import AsyncRattus (Stable)

-- | A strict version of UTCTime
data UTCTime' = UTCTime'
    { strictDay     :: !Day        -- ^ The day component
    , strictDayTime :: !DiffTime   -- ^ The time component
    } deriving (Eq, Ord)

-- | Convert from lazy UTCTime to UTCTime'
fromUTCTime :: UTCTime -> UTCTime'
fromUTCTime (UTCTime day time) = UTCTime'
    { strictDay = day
    , strictDayTime = time
    }

-- | Convert from UTCTime' to lazy UTCTime
toUTCTime :: UTCTime' -> UTCTime
toUTCTime (UTCTime' day time) = UTCTime day time

getCurrentStrictTime :: IO UTCTime'
getCurrentStrictTime = do
    UTCTime day time <- getCurrentTime
    return $ UTCTime' day time

diffUTCTime' :: UTCTime' -> UTCTime' -> NominalDiffTime
diffUTCTime' t1 t2 = diffUTCTime (toUTCTime t1) (toUTCTime t2)

addUTCTime' :: NominalDiffTime -> UTCTime' -> UTCTime'
addUTCTime' diff t = fromUTCTime (addUTCTime diff (toUTCTime t))

-- Make it show like regular UTCTime
instance Show UTCTime' where
    show = show . toUTCTime

instance Stable UTCTime'