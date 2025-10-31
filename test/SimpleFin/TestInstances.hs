{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module SimpleFin.TestInstances where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (POSIXTime)
import Test.QuickCheck

-- | Arbitrary instance for Text
instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  shrink = map T.pack . shrink . T.unpack

-- | Arbitrary instance for POSIXTime (seconds since Unix epoch)
instance Arbitrary POSIXTime where
  arbitrary = do
    -- Generate timestamps between year 2000 and 2030
    let minTime = 946684800  -- 2000-01-01
        maxTime = 1893456000  -- 2030-01-01
    seconds <- choose (minTime, maxTime)
    return $ fromIntegral (seconds :: Integer)

  shrink t =
    let seconds = floor t :: Integer
    in map fromIntegral $ shrink seconds