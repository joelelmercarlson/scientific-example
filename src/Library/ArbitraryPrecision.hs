{- 

ArbitraryPrecision.hs

Author: "Joel E Carlson" <joel.elmer.carlson@outlook.com>

-}

module Library.ArbitraryPrecision where

import Data.Scientific

mul :: Scientific -> Scientific -> Scientific
mul = (*)

add :: Scientific -> Scientific -> Scientific
add = (+)

scale :: Scientific -> Int -> Scientific
scale x n = x * 10 ^^ n

safeDiv :: Scientific -> Scientific -> Maybe Scientific
safeDiv _ 0 = Nothing
safeDiv a b = Just $ a / b

roundTo :: Int -> Scientific -> Scientific
roundTo n x =
  let factor = 10 ^^ n
  in fromInteger (round (x * factor)) / factor
