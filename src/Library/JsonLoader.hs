{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-

JsonLoader.hs

Author: "Joel E Carlson" <joel.elmer.carlson@outlook.com>

-}
module Library.JsonLoader (
  Amount(..)
  , decodeAmounts
  ) where

import Prelude hiding (lookup)
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Scientific
import Data.Text (Text)
import GHC.Generics (Generic)

data Amount = Amount
  { amount :: Scientific
  } deriving (Show, Eq, Generic)

instance ToJSON Amount
instance FromJSON Amount

decodeAmounts :: FilePath -> IO (Either String [Amount])
decodeAmounts fp = catchShowIO (BS.readFile fp)
  >>= return . either Left eitherDecode

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `Exception.catch` handleIOException
  where
    handleIOException :: IOException -> IO (Either String a)
    handleIOException = return . Left . show
