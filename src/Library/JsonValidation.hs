{-# LANGUAGE OverloadedStrings #-}

module Library.JsonValidation where

import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

data ValidationError
  = MissingField Text
  | NotANumber Text
  | OutOfRange Text
  | TooManyDecimals Text
  deriving (Show)

validateAmount :: Value -> Either ValidationError Scientific
validateAmount (Object o) =
  case KM.lookup (Key.fromText "amount") o of
    Nothing -> Left $ MissingField "amount"
    Just (Number n) ->
      if n < 0
      then Left $ OutOfRange "amount < 0"
      else if n > 1000000
      then Left $ OutOfRange "amount > 1,000,000"
      else if decimalPlaces n > 2
      then Left $ TooManyDecimals "amount has > 2 decimals"
      else Right n
    Just _ -> Left $ NotANumber "amount"
validateAmount _ = Left $ MissingField "amount"

decimalPlaces :: Scientific -> Int
decimalPlaces s =
  let e = base10Exponent s
  in if e >= 0 then 0 else negate e
