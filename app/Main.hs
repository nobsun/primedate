module Main where

import Data.Char
import Data.Time.Calendar
import System.Environment
import Math.NumberTheory.Primes

main :: IO ()
main = putStr . unlines . map showGregorian . primeDates . read . head =<< getArgs

primeDates :: Integer -> [Day]
primeDates n = filter isPrimeDate [jan1 .. addDays yds jan1]
  where yds = if isLeapYear n then 365 else 364
        jan1 = fromGregorian n 1 1

isPrimeDate :: Day -> Bool
isPrimeDate =  isPrime . read . filter isDigit . showGregorian
