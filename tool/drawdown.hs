#!/usr/bin/env stack
{-
stack --system-ghc --resolver lts-24.29 script --package time
-}

module Main (
  main
  ) where

import Control.Monad
import Data.Time
import Data.Time.Clock.POSIX

drawdown :: Double -> [Double]
drawdown 0 = []
drawdown x =
  let growth = 0.07
      draw   = 0.04
      period = x * (1 + growth - draw)
  in  period : drawdown period

main :: IO ()
main =
  let base = 1e6 
      raw  = take 5 $ drawdown base
      draw = map floor $ map (*0.04) raw
      year = zip raw draw
  in do
    utcTime <- getCurrentTime
    let tm = formatTime defaultTimeLocale "%Y-%m-%d" utcTime

    putStrLn $ tm
    void $ traverse (putStrLn . show) year
