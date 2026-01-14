{-# LANGUAGE OverloadedStrings #-}
{-

Main.hs

Author: "Joel E Carlson" <joel.elmer.carlson@outlook.com>

-}

module Main where

import Control.Monad
import Library.JsonLoader
import Library.JsonValidation

main :: IO ()
main =
  let jsPath = "dat/transact.json"
  in do
    ts <- decodeAmounts jsPath
    case ts of
      Left err -> putStrLn $ "error:" ++ err
      Right xs -> void $ traverse runAmount xs
