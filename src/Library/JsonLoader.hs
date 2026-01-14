{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-

JsonLoader.hs -- [{"amount": 1e6}]

Author: "Joel E Carlson" <joel.elmer.carlson@outlook.com>

-}
module Library.JsonLoader (
  Amount(..)
  , decodeAmount
  , runAmount
  ) where

import Prelude hiding (lookup)
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Scientific
import Data.Text (Text)
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import Library.DomainModel

data Amount = Amount
  { amount :: Scientific
  } deriving (Show, Eq, Generic)

instance ToJSON Amount
instance FromJSON Amount

decodeAmount :: FilePath -> IO (Either String [Amount])
decodeAmount fp = catchShowIO (BS.readFile fp)
  >>= return . either Left eitherDecode

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show

runAmount :: Amount -> IO ()
runAmount Amount{..} =
  getCurrentTime >>= \utcTime -> do
  let tm = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" utcTime
  putStrLn $ tm
  putStrLn $ "Future Value: " ++ (show $ futureValue amount 0.07 5)
  print $ take 5 $ drawDownModel $ mkTransaction amount
