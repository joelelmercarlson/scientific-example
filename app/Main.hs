{-# LANGUAGE OverloadedStrings #-}
{-

Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@outlook.com>

-}

module Main where

import Library.JsonValidation

main :: IO ()
main =
  let json1 = "{\"amount\": 1e6}"
      json2 = "{\"amount\": 123.3456}"
      json3 = "{\"amount\": -1.0}"
      json4 = "{\"value\": 10}"
      json5 = "{\"amount\": abc}"
  in do
    runExample json1
    runExample json2
    runExample json3
    runExample json4
    runExample json5
