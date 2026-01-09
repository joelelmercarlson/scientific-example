{-# LANGUAGE OverloadedStrings #-}
{-

JsonValidation.hs

Author: "Joel E Carlson" <joel.elmer.carlson@outlook.com>

-}
module Library.JsonValidation where

import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
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
  utcTime <- getCurrentTime
  let tm = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utcTime
  putStrLn $ tm
  case decode (BL.pack $ T.unpack raw) :: Maybe Value of
    Nothing -> putStrLn "Invalid JSON"
    Just v ->
      case validateAmount v of
        Right amt -> do
          putStrLn $ "Valid amount: " ++ show amt
          putStrLn $ "Future value: " ++ (show $ futureValue amt 5)
          print $ take 5 $ drawDownModel $ mkTransaction amt
        Left err -> putStrLn $ "Validation Error: " ++ show err
  
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
