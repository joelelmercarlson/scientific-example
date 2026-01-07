{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Library.DomainModel where

import GHC.Generics
import Data.Aeson
import Data.Scientific
import qualified Data.Scientific as S
import Library.ArbitraryPrecision

data Transaction = Transaction
  { amount  :: Scientific
  , taxRate :: Scientific  -- e.g., 0.075 for 7.5%
  } deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON   Transaction

-- Domain rule: total = amount + (amount * taxRate)
total :: Transaction -> Scientific
total t = amount t + (amount t * taxRate t)

-- Domain rule: round to 2 decimals for display
totalRounded :: Transaction -> Scientific
totalRounded = roundTo 2 . total
