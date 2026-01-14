{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-

JsonValidation.hs -- {"amount": 1e6}

Author: "Joel E Carlson" <joel.elmer.carlson@outlook.com>

-}
module Library.JsonValidation where

import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Library.ArbitraryPrecision
import Library.DomainModel

data ValidationError
  = MissingField Text
  | NotANumber Text
  | OutOfRange Text
  | TooManyDecimals Text
  deriving (Show)

runExample :: Text -> IO ()
runExample raw = do
  case decode (BL.pack $ T.unpack raw) :: Maybe Value of
    Nothing -> putStrLn "Invalid JSON"
    Just v ->
      case validateAmount v of
        Right amt -> putStrLn $ "Valid amount: " ++ show amt
        Left err  -> putStrLn $ "Validation Error: " ++ show err
  
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
