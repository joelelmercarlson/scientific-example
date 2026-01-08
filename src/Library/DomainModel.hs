{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{- 

DomainModel.hs

Author: "Joel E Carlson" <joel.elmer.carlson@outlook.com>

-}
module Library.DomainModel where

import GHC.Generics
import Data.Aeson
import Data.Scientific
import qualified Data.Scientific as S
import Library.ArbitraryPrecision

data Transaction = Transaction
  { amount     :: Scientific
  , growthRate :: Scientific
  , taxRate    :: Scientific  -- e.g., 0.075 for 7.5%
  } deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON   Transaction

-- | future value
drawDownAmount :: Scientific -> [Scientific]
drawDownAmount 0 = []
drawDownAmount x =
  let growth = scientific 7 (-2)
      rate   = scientific 4 (-2)
      period = x * (1 + growth - rate)
  in period : drawDownAmount period

-- | future value
drawDownModel :: Transaction -> [Transaction]
drawDownModel (Transaction 0 _ _ ) = []
drawDownModel t =
  let amt = (amount t) * (1.0 + (growthRate t) - (taxRate t))
      nxt = mkTransaction amt
  in nxt : drawDownModel nxt
  
mkTransaction :: Scientific -> Transaction
mkTransaction t = Transaction {
  amount = t
  , growthRate = scientific 7 (-2)
  , taxRate = scientific 4 (-2)
  }

-- Domain rule: total = amount + (amount * taxRate)
total :: Transaction -> Scientific
total t = amount t + (amount t * taxRate t)

-- Domain rule: round to 2 decimals for display
totalRounded :: Transaction -> Scientific
totalRounded = roundTo 2 . total
