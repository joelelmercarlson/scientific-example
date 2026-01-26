#!/usr/bin/env stack
{-
stack --system-ghc script --resolver lts-24.29 --package text --package time
-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Time
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf

main :: IO ()
main = do
  ooo <- getContents
  time <- getCurrentTime
  let year  = formatTime defaultTimeLocale "%Y" time
      month = formatTime defaultTimeLocale "%B" time
      day   = read ooo :: Int
  printf "Joel is on PTO, "
  printf "until %s %d%s.\n" month day $ dateTense day

dateTense:: Int -> Text
dateTense x
  | x == 1 || x == 21 || x == 31 = "st"
  | x == 2 || x == 22 = "nd"
  | x == 3 || x == 23 = "rd"
  | otherwise = "th"

esc :: String -> Text
esc x = T.pack $ "\x1b[" ++ x ++ ";1m"

black :: Text -> Text
black = T.append (esc "30")

red :: Text -> Text
red = T.append (esc "31")

green :: Text -> Text
green = T.append (esc "32")

yellow :: Text -> Text
yellow = T.append (esc "33")

blue :: Text -> Text
blue = T.append (esc "34")

magenta :: Text -> Text
magenta = T.append (esc "35")

cyan :: Text -> Text
cyan = T.append (esc "36")

white :: Text -> Text
white = T.append (esc "37")

clear :: Text -> Text
clear = T.append (esc "0")
