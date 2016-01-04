module Main where

import Data.Char
import Data.Time.Calendar
import System.Environment
import Math.NumberTheory.Primes

main :: IO ()
main = putStr . unlines . map showGregorian . primeDates . read . head =<< getArgs

primeDates :: Integer -> [Day]
primeDates n = filter isPrimeDate [jan01 .. dec31]
  where jan01 = fromGregorian n 1 1
        dec31 = fromGregorian n 12 31

isPrimeDate :: Day -> Bool
isPrimeDate =  isPrime . read . filter isDigit . showGregorian
