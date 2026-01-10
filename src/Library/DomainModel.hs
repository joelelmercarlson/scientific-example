{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
  , delta      :: Scientific
  , growthRate :: Scientific
  , taxRate    :: Scientific  -- e.g., 0.075 for 7.5%
  , timePeriod :: Int
  } deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON   Transaction

-- | future value
drawDownModel :: Transaction -> [Transaction]
drawDownModel (Transaction 0 _ _ _ _) = []
drawDownModel Transaction{..} =
  let nxt = mkTransaction' (1 + timePeriod) amount
  in nxt : drawDownModel nxt
  
futureValue :: Scientific -> Int -> Scientific
futureValue s p = s * (1.0 + 0.075) ^^ p

mkTransaction :: Scientific -> Transaction
mkTransaction = mkTransaction' 0

mkTransaction' :: Int -> Scientific -> Transaction
mkTransaction' t s =
  let fv     = s * (1.0 + growth - tax)
      dt     = fv - s
      growth = scientific 7 (-2)
      tax    = scientific 4 (-2)
  in Transaction { amount     = fv
                 , delta      = dt
                 , growthRate = growth
                 , taxRate    = tax
                 , timePeriod = t
                 }

-- Domain rule: total = amount + (amount * taxRate)
total :: Transaction -> Scientific
total t = amount t + (amount t * taxRate t)

-- Domain rule: round to 2 decimals for display
totalRounded :: Transaction -> Scientific
totalRounded = roundTo 2 . total
