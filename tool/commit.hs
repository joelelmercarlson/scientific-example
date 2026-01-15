#!/usr/bin/env stack
{- stack
--system-ghc
--resolver lts-24.28 script
--package process
--package time
-}
module Main (main) where

import Data.List
import Data.Maybe
import Data.Time.Clock.POSIX
import System.Environment
import System.Process

-- | git
-- git config --global user.email "joel.elmer.carlson@outlook.com"
-- git config --global user.name "Joel E Carlson"
-- git config --global credential.helper store
--

main :: IO ()
main = do
  xs <- getArgs
  tm <- floor `fmap` getPOSIXTime

  let arg  = safeIndex 0 xs
      note = fromMaybe (show tm) (arg)
      cmd  = "git commit -m \"" ++ note ++ "\""

  callCommand "git add ."
  callCommand cmd
  callCommand "git push"
  print $ "message: " ++ note

safeIndex :: Int -> [a] -> Maybe a
safeIndex n xs = fmap fst . uncons $ drop n xs
