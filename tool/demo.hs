#!/usr/bin/env stack
{- stack
--system-ghc
--resolver lts-24.28 script
--package scientific
-}
module Main where

import Data.Scientific

-- | Scientific numbers
main :: IO ()
main =
  let a = scientific 1245 0
      b = scientific 1245 (-2)
      c = read "1.2345e10" :: Scientific
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
