{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Scientific
import qualified Data.ByteString.Lazy.Char8 as BL
import Library.ArbitraryPrecision
import Library.JsonValidation

main :: IO ()
main =
  let a = scientific 1245 0
      b = scientific 1245 (-2)
      c = read "1.2345e10" :: Scientific
      json1 = "{\"amount\": 123.45}"
      json2 = "{\"amount\": 123.3456}"
      json3 = "{\"amount\": -1.0}"
      json4 = "{\"value\": 10}"
      json5 = "{\"amount\": 5}"
  in do
    putStrLn "Basic Scientific values:"
    print a
    print b
    print c
    putStrLn "\nConversion:"
    print (toRealFloat c :: Double)
    print (floatingOrInteger c :: Either Double Integer)
    putStrLn "\nCoefficient and exponent:"
    print (coefficient b, base10Exponent b)
    putStrLn "\nConversion:"
    print $ roundTo 2 a

    runExample "valid amount" json1
    runExample "too many decimals" json2
    runExample "negative amount" json3
    runExample "missing field" json4
    runExample "not a number" json5


runExample :: String -> String -> IO ()
runExample label raw = do
  putStrLn $ "\n--- " ++ label ++ " ---"
  case decode (BL.pack raw) :: Maybe Value of
    Nothing -> putStrLn "Invalid JSON"
    Just v ->
      case validateAmount v of
        Right amt -> putStrLn $ "Valid amount: " ++ show amt
        Left err -> putStrLn $ "Validation Error: " ++ show err
  
  
